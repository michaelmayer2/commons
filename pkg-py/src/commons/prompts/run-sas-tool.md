Run SAS code on the SAS server, in a SAS session of your own. Use it when the data a question needs is held in SAS libraries rather than in the tables you can query, or when a SAS procedure is the most direct way to derive the answer. SAS code, its listing, and its log messages are visible only to you. The user cannot access or interact with this session. Never direct them to run code or inspect its datasets; perform follow-up analysis yourself and report the result in your response. Your session persists across calls: WORK datasets, macro variables, and options you set remain available. Data-frame results from other tools are uploaded before each call as WORK datasets named after their handles (WORK.R1, WORK.R2, ...). Trusted SAS calculations run in a separate session and cannot be reached from here.

Rules:
- Work incrementally: each call should do one small, well-defined task.
- Print brief summaries (PROC MEANS, PROC FREQ, or PROC PRINT with OBS=) rather than whole datasets.
- The tool returns text only, so ODS graphics are not shown.
- Do not use this tool to talk to the user; explanations belong in your reply.
- Create datasets only in WORK.
