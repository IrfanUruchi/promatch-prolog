% web_layout.pl – shared layout + styling (app shell)

:- module(web_layout, [
    app_shell//2
]).

:- use_module(library(http/html_write)).


base_style -->
    html(style('
:root {
  color-scheme: dark;
  --bg: #020617;
  --bg-elevated: #020617;
  --bg-card: #020617;
  --border-subtle: #111827;
  --border-strong: #4b5563;
  --text-main: #e5e7eb;
  --text-muted: #9ca3af;
  --accent: #4f46e5;
  --accent-soft: rgba(79,70,229,0.2);
  --accent-strong: #6366f1;
  --danger: #f97373;
  --radius-lg: 18px;
  --radius-pill: 999px;
  --shadow-soft: 0 18px 40px rgba(15,23,42,0.75);
}

*,
*::before,
*::after {
  box-sizing: border-box;
}

html, body {
  margin: 0;
  padding: 0;
}

body {
  font-family: -apple-system, BlinkMacSystemFont, "SF Pro Text",
               system-ui, "Segoe UI", sans-serif;
  background: radial-gradient(circle at top, #020617 0, #020617 40%, #020617 100%);
  color: var(--text-main);
  font-size: 14px;
  -webkit-font-smoothing: antialiased;
}

/* App shell */

.app-shell {
  min-height: 100vh;
  display: flex;
  flex-direction: column;
}

.app-header {
  position: sticky;
  top: 0;
  z-index: 10;
  backdrop-filter: blur(18px);
  background: linear-gradient(to bottom,
              rgba(15,23,42,0.98) 0,
              rgba(15,23,42,0.92) 60%,
              rgba(15,23,42,0.88) 100%);
  border-bottom: 1px solid rgba(148,163,184,0.18);
}

.app-header-inner {
  max-width: 1160px;
  margin: 0 auto;
  padding: 10px 20px;
  display: flex;
  align-items: center;
  gap: 14px;
}

.app-logo-mark {
  width: 26px;
  height: 26px;
  border-radius: 10px;
  background: radial-gradient(circle at 30% 0%,
               #a5b4fc 0, #4f46e5 40%, #020617 100%);
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 14px;
  color: white;
  box-shadow: 0 10px 28px rgba(79,70,229,0.55);
}

.app-logo-text {
  display: flex;
  flex-direction: column;
  gap: 1px;
}

.app-logo-title {
  font-size: 15px;
  font-weight: 600;
  letter-spacing: 0.02em;
}

.app-logo-sub {
  font-size: 12px;
  color: var(--text-muted);
}

.app-nav {
  margin-left: auto;
  display: flex;
  gap: 6px;
  align-items: center;
}

.nav-link {
  padding: 5px 12px;
  border-radius: 999px;
  font-size: 13px;
  color: var(--text-muted);
  text-decoration: none;
  border: 1px solid transparent;
  transition: all 0.16s ease-out;
  white-space: nowrap;
}

.nav-link:hover {
  color: var(--text-main);
  border-color: rgba(148,163,184,0.35);
  background: rgba(15,23,42,0.92);
}

.nav-link.active {
  color: var(--text-main);
  border-color: rgba(148,163,184,0.85);
  background: radial-gradient(circle at top,
             rgba(79,70,229,0.30) 0,
             rgba(15,23,42,0.98) 52%);
  box-shadow: 0 10px 30px rgba(15,23,42,0.9);
}

/* Layout */

.main {
  flex: 1;
}

.main-inner {
  max-width: 1160px;
  margin: 18px auto 32px auto;
  padding: 0 20px 32px 20px;
}

/* Hero */

.hero {
  display: grid;
  grid-template-columns: minmax(0, 1.3fr) minmax(0, 1fr);
  gap: 24px;
  margin-bottom: 22px;
}

.hero-left-title {
  font-size: 30px;
  font-weight: 640;
  letter-spacing: -0.03em;
}

.hero-left-sub {
  margin-top: 10px;
  font-size: 14px;
  color: var(--text-muted);
  max-width: 460px;
}

.hero-pills {
  margin-top: 16px;
  display: flex;
  flex-wrap: wrap;
  gap: 6px;
}

.hero-pill {
  font-size: 12px;
  border-radius: 999px;
  padding: 4px 10px;
  border: 1px solid rgba(148,163,184,0.35);
  color: var(--text-muted);
}

.hero-panel {
  background: radial-gradient(circle at 0 0,
                rgba(79,70,229,0.24) 0,
                #020617 38%, #020617 100%);
  border-radius: 20px;
  border: 1px solid rgba(148,163,184,0.32);
  box-shadow: var(--shadow-soft);
  padding: 14px 16px 16px 16px;
  display: flex;
  flex-direction: column;
  gap: 12px;
}

.hero-panel-col {
  display: flex;
  flex-direction: column;
  gap: 8px;
}

.hero-panel-label {
  font-size: 12px;
  color: var(--text-muted);
}

.hero-panel-value {
  font-size: 14px;
}

.hero-panel-value b {
  font-size: 21px;
  font-weight: 620;
}

.hero-panel-chip-row {
  display: flex;
  flex-wrap: wrap;
  gap: 6px;
}

.hero-panel-chip {
  font-size: 12px;
  border-radius: 999px;
  padding: 4px 10px;
  background: rgba(15,23,42,0.96);
  border: 1px solid rgba(148,163,184,0.45);
}

/* Sections / cards */

.section-card {
  background: rgba(15,23,42,0.98);
  border-radius: var(--radius-lg);
  border: 1px solid rgba(15,23,42,1);
  box-shadow: 0 14px 40px rgba(15,23,42,0.9);
  padding: 18px 18px 18px 18px;
  margin-bottom: 18px;
}

.section-header-row {
  display: flex;
  align-items: baseline;
  justify-content: space-between;
  gap: 8px;
  margin-bottom: 10px;
}

.section-header-row h2 {
  font-size: 17px;
  font-weight: 600;
  margin: 0;
}

.section-header-sub {
  font-size: 13px;
  color: var(--text-muted);
}

.section-body {
  font-size: 14px;
  color: var(--text-main);
}

/* Grid */

.grid-2 {
  display: grid;
  grid-template-columns: minmax(0,1fr) minmax(0,1fr);
  gap: 16px;
  margin-bottom: 18px;
}

/* Stats */

.stats-row {
  display: flex;
  flex-wrap: wrap;
  gap: 10px;
}

.stat-card {
  flex: 1 1 0;
  min-width: 130px;
  padding: 10px 12px;
  border-radius: 16px;
  border: 1px solid rgba(30,64,175,0.9);
  background: radial-gradient(circle at top left,
              rgba(79,70,229,0.28) 0,
              rgba(15,23,42,1) 52%);
}

.stat-label {
  font-size: 12px;
  color: var(--text-muted);
}

.stat-value {
  font-size: 22px;
  font-weight: 630;
  margin-top: 4px;
}

.stat-caption {
  font-size: 12px;
  color: var(--text-muted);
  margin-top: 4px;
}

/* Buttons */

.btn-primary,
.btn-ghost,
.btn-link,
.btn-ghost-small {
  font-family: inherit;
  cursor: pointer;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  gap: 4px;
  text-decoration: none;
}

.btn-primary {
  padding: 7px 16px;
  font-size: 13px;
  border-radius: var(--radius-pill);
  border: 1px solid rgba(129,140,248,0.9);
  background: radial-gradient(circle at top left,
              rgba(129,140,248,0.32) 0,
              rgba(79,70,229,1) 45%,
              rgba(15,23,42,1) 100%);
  color: white;
  box-shadow: 0 12px 34px rgba(79,70,229,0.65);
}

.btn-primary:hover {
  filter: brightness(1.05);
}

.btn-ghost {
  padding: 6px 13px;
  font-size: 13px;
  border-radius: var(--radius-pill);
  border: 1px solid rgba(148,163,184,0.45);
  background: rgba(15,23,42,0.96);
  color: var(--text-main);
}

.btn-ghost:hover {
  border-color: rgba(148,163,184,0.85);
}

.btn-ghost-small {
  padding: 4px 9px;
  font-size: 12px;
  border-radius: var(--radius-pill);
  border: 1px solid rgba(148,163,184,0.4);
  background: rgba(15,23,42,0.98);
  color: var(--text-main);
}

.btn-ghost-small:hover {
  border-color: rgba(148,163,184,0.85);
}

.btn-link {
  padding: 0;
  font-size: 13px;
  color: #a5b4fc;
  border: none;
  background: none;
}

.btn-link:hover {
  text-decoration: underline;
}

/* Tables */

.data-table {
  width: 100%;
  border-collapse: collapse;
  font-size: 13px;
  margin-top: 4px;
}

.data-table thead tr {
  border-bottom: 1px solid rgba(31,41,55,1);
}

.data-table th,
.data-table td {
  padding: 7px 8px;
  text-align: left;
}

.data-table th {
  font-weight: 500;
  font-size: 12px;
  color: var(--text-muted);
}

.data-table tbody tr {
  border-bottom: 1px solid rgba(15,23,42,0.7);
}

.data-table tbody tr:last-child {
  border-bottom-color: transparent;
}

.data-table tbody tr:hover {
  background: rgba(17,24,39,0.9);
}

/* Misc */

.muted {
  color: var(--text-muted);
  font-size: 12px;
}

.event-name {
  font-weight: 500;
}

.radio-row {
  display: inline-flex;
  flex-wrap: wrap;
  align-items: center;
  gap: 4px 10px;
  padding: 4px 8px;
  border-radius: var(--radius-pill);
  background: rgba(15,23,42,0.96);
  border: 1px solid rgba(55,65,81,1);
}

.radio-row input[type="radio"] {
  margin-right: 2px;
}

/* Status labels */

.status-label {
  font-size: 12px;
  padding: 2px 8px;
  border-radius: var(--radius-pill);
  border: 1px solid rgba(148,163,184,0.4);
  color: var(--text-muted);
}

/* Mini profile + score card for why_match */

.mini-profile h3 {
  font-size: 14px;
  margin: 0 0 4px 0;
}

.mini-profile p {
  margin: 0 0 3px 0;
  font-size: 13px;
}

.score-block {
  margin-top: 12px;
  padding: 10px 12px;
  border-radius: 14px;
  border: 1px solid rgba(30,64,175,0.9);
  background: radial-gradient(circle at 0 0,
              rgba(79,70,229,0.28) 0,
              rgba(15,23,42,1) 52%);
}

.score-block span {
  font-size: 12px;
}

.score-block div {
  font-size: 34px;
  font-weight: 650;
  margin-top: 4px;
}

.reason-list {
  list-style: disc;
  padding-left: 18px;
  font-size: 13px;
}

/* Textareas */

textarea {
  font-family: -apple-system, BlinkMacSystemFont, "SF Pro Text",
               system-ui, "Segoe UI", sans-serif;
  font-size: 13px;
  line-height: 1.4;
}

/* Responsive tweaks */

@media (max-width: 900px) {
  .app-header-inner {
    flex-wrap: wrap;
  }
  .app-nav {
    width: 100%;
    justify-content: flex-start;
    margin-left: 0;
    margin-top: 4px;
    overflow-x: auto;
  }
  .hero {
    grid-template-columns: minmax(0,1fr);
  }
  .grid-2 {
    grid-template-columns: minmax(0,1fr);
  }
}

@media (max-width: 640px) {
  .main-inner {
    padding: 0 14px 24px 14px;
  }
  .section-card {
    padding: 14px 14px 16px 14px;
  }
}
    ')).

% ---------------------------------------------------
% App shell
% ---------------------------------------------------

app_shell(ActiveSection, Inner) -->
    base_style,
    html([
        div(class('app-shell'), [
            \app_header(ActiveSection),
            div(class(main), [
                div(class('main-inner'), [
                    \Inner
                ])
            ])
        ])
    ]).

app_header(Active) -->
    html(
        header(class('app-header'), [
            div(class('app-header-inner'), [
                div(class('app-logo-mark'), span('P')),
                div(class('app-logo-text'), [
                    div(class('app-logo-title'), 'ProMatch'),
                    div(class('app-logo-sub'),
                        'Logical & Functional Programming · Matchmaking workspace')
                ]),
                nav(class('app-nav'), [
                    \nav_link(Active, home,          '/',              'Overview'),
                    \nav_link(Active, events,        '/events',        'Events'),
                    \nav_link(Active, event_console, '/event_console', 'Console'),
                    \nav_link(Active, ai,            '/ai',            'AI'),
                    \nav_link(Active, analytics,     '/analytics',     'Analytics'),
                    \nav_link(Active, constraints,   '/constraints',   'Constraints')
                ])
            ])
        ])
    ).

nav_link(Active, Id, Href, Label) -->
    {
        ( Active == Id -> Class = 'nav-link active'
        ;                 Class = 'nav-link'
        )
    },
    html(a([href(Href), class(Class)], Label)).
