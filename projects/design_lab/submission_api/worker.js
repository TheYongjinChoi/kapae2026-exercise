// KAPAE student submission API — Cloudflare Worker (ES module)
// Keeps the GitHub write token on the server. Students only POST their QMD.

const OWNER = "TheYongjinChoi";
const REPO = "kapae2026-exercise";
const BRANCH = "main";
const API_VERSION = "2022-11-28";
const MAX_BYTES = 350000;

function jsonResponse(body, status = 200) {
  return new Response(JSON.stringify(body), {
    status,
    headers: {
      "content-type": "application/json; charset=utf-8",
      "access-control-allow-origin": "*",
      "access-control-allow-headers": "content-type",
      "access-control-allow-methods": "POST, OPTIONS"
    }
  });
}

function utf8ToBase64(text) {
  const bytes = new TextEncoder().encode(text);
  let binary = "";
  const chunk = 0x8000;
  for (let i = 0; i < bytes.length; i += chunk) {
    binary += String.fromCharCode(...bytes.subarray(i, i + chunk));
  }
  return btoa(binary);
}

async function githubRequest(path, env, init = {}) {
  const headers = new Headers(init.headers || {});
  headers.set("accept", "application/vnd.github+json");
  headers.set("authorization", `Bearer ${env.GITHUB_TOKEN}`);
  headers.set("x-github-api-version", API_VERSION);
  headers.set("user-agent", "kapae2026-submission-worker");
  return fetch(`https://api.github.com${path}`, { ...init, headers });
}

export default {
  async fetch(request, env) {
    if (request.method === "OPTIONS") return jsonResponse({ ok: true });
    if (request.method !== "POST") return jsonResponse({ error: "POST only" }, 405);

    if (!env.GITHUB_TOKEN) return jsonResponse({ error: "Server is missing GITHUB_TOKEN" }, 500);

    let payload;
    try {
      payload = await request.json();
    } catch {
      return jsonResponse({ error: "Invalid JSON" }, 400);
    }

    const studentId = String(payload.student_id || "").trim();
    const method = String(payload.method || "").trim().toLowerCase();
    const content = String(payload.content || "");
    const courseKey = String(payload.course_key || "");

    if (env.COURSE_KEY && courseKey !== env.COURSE_KEY) {
      return jsonResponse({ error: "Invalid course key" }, 403);
    }
    if (!/^[A-Za-z0-9_-]{3,30}$/.test(studentId)) {
      return jsonResponse({ error: "Invalid student_id" }, 400);
    }
    if (!/^(prediction|dml|did|iv|matching|causal_forest|rd)$/.test(method)) {
      return jsonResponse({ error: "Invalid method" }, 400);
    }
    if (!content.includes("student-id:") || !content.includes("design-method:")) {
      return jsonResponse({ error: "Generated Step 2 QMD required" }, 400);
    }
    if (new TextEncoder().encode(content).length > MAX_BYTES) {
      return jsonResponse({ error: "QMD is too large" }, 413);
    }

    const filename = `${studentId}_${method}.qmd`;
    const repoPath = `projects/submissions/${filename}`;
    const contentApiPath = `/repos/${OWNER}/${REPO}/contents/${repoPath}`;

    // If the student resubmits, GitHub requires the existing blob SHA.
    let existingSha = null;
    const existing = await githubRequest(`${contentApiPath}?ref=${encodeURIComponent(BRANCH)}`, env);
    if (existing.status === 200) {
      const current = await existing.json();
      existingSha = current.sha;
    } else if (existing.status !== 404) {
      return jsonResponse({ error: "Could not check existing submission", github_status: existing.status }, 502);
    }

    const body = {
      message: `Student submission: ${studentId} (${method})`,
      content: utf8ToBase64(content),
      branch: BRANCH
    };
    if (existingSha) body.sha = existingSha;

    const saved = await githubRequest(contentApiPath, env, {
      method: "PUT",
      headers: { "content-type": "application/json" },
      body: JSON.stringify(body)
    });

    if (!saved.ok) {
      const detail = await saved.text();
      return jsonResponse({ error: "GitHub write failed", github_status: saved.status, detail: detail.slice(0, 1000) }, 502);
    }

    const result = await saved.json();
    return jsonResponse({
      ok: true,
      path: repoPath,
      commit: result?.commit?.sha || null,
      updated: Boolean(existingSha)
    });
  }
};
