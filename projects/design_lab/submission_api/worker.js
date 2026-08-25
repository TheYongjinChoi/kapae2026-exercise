// KAPAE student submission API — Cloudflare Worker (ES module)
// Accepts both legacy QMD submissions and rendered HTML submissions.
// Rendered HTML is written directly to docs/students/ for GitHub Pages.

const OWNER = "TheYongjinChoi";
const REPO = "kapae2026-exercise";
const BRANCH = "main";
const API_VERSION = "2022-11-28";
const MAX_QMD_BYTES = 500000;
const MAX_HTML_BYTES = 20000000;

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

async function putFile(repoPath, text, commitMessage, env) {
  const apiPath = `/repos/${OWNER}/${REPO}/contents/${repoPath}`;

  let sha = null;
  const existing = await githubRequest(`${apiPath}?ref=${encodeURIComponent(BRANCH)}`, env);

  if (existing.status === 200) {
    const current = await existing.json();
    sha = current.sha;
  } else if (existing.status !== 404) {
    return {
      ok: false,
      status: existing.status,
      error: "Could not check existing file"
    };
  }

  const body = {
    message: commitMessage,
    content: utf8ToBase64(text),
    branch: BRANCH
  };
  if (sha) body.sha = sha;

  const saved = await githubRequest(apiPath, env, {
    method: "PUT",
    headers: { "content-type": "application/json" },
    body: JSON.stringify(body)
  });

  if (!saved.ok) {
    const detail = await saved.text();
    return {
      ok: false,
      status: saved.status,
      error: "GitHub write failed",
      detail: detail.slice(0, 1000)
    };
  }

  const result = await saved.json();
  return {
    ok: true,
    updated: Boolean(sha),
    commit: result?.commit?.sha || null
  };
}

export default {
  async fetch(request, env) {
    if (request.method === "OPTIONS") return jsonResponse({ ok: true });
    if (request.method !== "POST") return jsonResponse({ error: "POST only" }, 405);

    if (!env.GITHUB_TOKEN) {
      return jsonResponse({ error: "Server is missing GITHUB_TOKEN" }, 500);
    }

    let payload;
    try {
      payload = await request.json();
    } catch {
      return jsonResponse({ error: "Invalid JSON" }, 400);
    }

    const studentId = String(payload.student_id || "").trim();
    const method = String(payload.method || "").trim().toLowerCase();
    const courseKey = String(payload.course_key || "");
    const content = String(payload.content || "");
    const submissionType = String(payload.submission_type || "qmd").trim().toLowerCase();
    const researchTitle = String(payload.research_title || "").trim();

    if (env.COURSE_KEY && courseKey !== env.COURSE_KEY) {
      return jsonResponse({ error: "Invalid course key" }, 403);
    }

    if (!/^[A-Za-z0-9_-]{3,30}$/.test(studentId)) {
      return jsonResponse({ error: "Invalid student_id" }, 400);
    }

    if (!/^(prediction|dml|did|iv|matching|causal_forest|rd)$/.test(method)) {
      return jsonResponse({ error: "Invalid method" }, 400);
    }

    const size = new TextEncoder().encode(content).length;

    // Backward-compatible QMD submission.
    if (submissionType === "qmd") {
      if (!content.includes("student-id:") || !content.includes("design-method:")) {
        return jsonResponse({ error: "Generated Step 2 QMD required" }, 400);
      }
      if (size > MAX_QMD_BYTES) {
        return jsonResponse({ error: "QMD is too large" }, 413);
      }

      const path = `projects/submissions/${studentId}_${method}.qmd`;
      const result = await putFile(
        path,
        content,
        `Student source submission: ${studentId} (${method})`,
        env
      );

      if (!result.ok) {
        return jsonResponse({
          error: result.error,
          github_status: result.status,
          detail: result.detail || null
        }, 502);
      }

      return jsonResponse({
        ok: true,
        submission_type: "qmd",
        path,
        commit: result.commit,
        updated: result.updated
      });
    }

    if (submissionType !== "html") {
      return jsonResponse({ error: "Invalid submission_type" }, 400);
    }

    if (!/<html[\s>]/i.test(content)) {
      return jsonResponse({ error: "Rendered HTML required" }, 400);
    }

    if (size > MAX_HTML_BYTES) {
      return jsonResponse({ error: "HTML is too large" }, 413);
    }

    const stem = `${studentId}_${method}`;
    const htmlPath = `docs/students/${stem}.html`;
    const metaPath = `docs/meta/${stem}.json`;

    const htmlResult = await putFile(
      htmlPath,
      content,
      `Rendered student page: ${studentId} (${method})`,
      env
    );

    if (!htmlResult.ok) {
      return jsonResponse({
        error: htmlResult.error,
        github_status: htmlResult.status,
        detail: htmlResult.detail || null
      }, 502);
    }

    const metadata = JSON.stringify({
      student_id: studentId,
      method,
      research_title: researchTitle || "연구 제목 미입력",
      html_path: `students/${stem}.html`,
      submitted_at: new Date().toISOString()
    }, null, 2) + "\n";

    const metaResult = await putFile(
      metaPath,
      metadata,
      `Student gallery metadata: ${studentId} (${method})`,
      env
    );

    if (!metaResult.ok) {
      return jsonResponse({
        error: metaResult.error,
        github_status: metaResult.status,
        detail: metaResult.detail || null,
        html_saved: true,
        html_path: htmlPath
      }, 502);
    }

    return jsonResponse({
      ok: true,
      submission_type: "html",
      path: htmlPath,
      metadata_path: metaPath,
      commit: metaResult.commit,
      updated: htmlResult.updated
    });
  }
};
