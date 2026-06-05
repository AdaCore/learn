# Git workflow (suggested)

> These are *suggested* recommendations for branch naming and commit messages.
> They are not mandatory, but following them keeps the history consistent and
> reviewable.

## Branch naming

Development work happens on a task-specific *topic branch* created off the
default branch (`main`), never directly on `main`. Two top-level forms are used,
selected by whether an issue exists:

```
dev/issue/<ticket>/<descriptor>[/<YYYY-MM-DD>]                # an issue exists
dev/topic/<area>/<sub-area>/.../<descriptor>[/<YYYY-MM-DD>]   # no issue
```

- `issue` and `topic` are literal selectors.
- `<descriptor>` is a short, hyphenated slug naming the specific work (for
  example `multi-project-structure` or `keyboard-nav`). It is what distinguishes
  this branch from other work in the same area.
- `<date>` is `YYYY-MM-DD`, optional, placed at the end so branches group by
  topic (the date is the leaf differentiator).
- Use the proper name for a known component (`compile_blocks`,
  `github_actions`); use hyphens for free-text descriptors (`multi-project`).

For the learning material, the only top-level distinction that matters is
whether the work touches *content* or *infrastructure*:

- `content` -- ReST authoring (what readers read). Sub-areas mirror the
  `content/` tree:
  - `dev/topic/content/general/<descriptor>[/<date>]` -- cross-cutting work
    spanning several courses, or site-wide editorial passes
  - `dev/topic/content/courses/<course>/<descriptor>[/<date>]`
  - `dev/topic/content/labs/<lab>/<descriptor>[/<date>]`
  - `dev/topic/content/booklets/<booklet>/<descriptor>[/<date>]`

- `infrastructure` -- everything that is *not* ReST content authoring. This
  includes everything under `frontend/` (the TypeScript widget, SCSS, webpack,
  the Sphinx plugins, the Makefile) as well as CI and the VM setup. Sub-area is
  the component:
  - `dev/topic/infrastructure/sphinx/<descriptor>[/<date>]`
  - `dev/topic/infrastructure/compile_blocks/<descriptor>[/<date>]`
  - `dev/topic/infrastructure/github_actions/<descriptor>[/<date>]`
  - `dev/topic/infrastructure/widget/<descriptor>[/<date>]`
  - `dev/topic/infrastructure/vagrant/<descriptor>[/<date>]`
  - `dev/topic/infrastructure/pnpm/<descriptor>[/<date>]`

### Feature sub-branches (suggested for bigger implementations)

For larger work split into multiple phases or work items, a *main + feature
branches* approach is recommended. The topic branch above becomes the
integration branch by appending `/main`, and each work item gets its own feature
branch by appending `/feat/<feature-slug>`:

```
<topic-root>/main             # integration branch for the whole topic
<topic-root>/feat/<slug>      # one per feature / work item
```

For example, with a topic root of
`dev/topic/infrastructure/pnpm/migration/2026-05-25`:

```
dev/topic/infrastructure/pnpm/migration/2026-05-25/main
dev/topic/infrastructure/pnpm/migration/2026-05-25/feat/remove-yarn
dev/topic/infrastructure/pnpm/migration/2026-05-25/feat/lock-file
```

Each feature branch is merged back into `<topic-root>/main` with a non
fast-forward merge (`git merge --no-ff`), using the message
`Merge feat/<feature-slug>: <summary>`. The whole topic is then opened as a
single pull request against `main`.

For small, single-purpose changes the feature branches can be omitted: work
directly on the topic branch (without the `/main` suffix).

## Commit messages

Subject lines carry a canonical tag identifying intent, followed by a colon and
an imperative description (up to 72 characters):

```
<Tag>: <imperative short description>
```

- Use a colon (`Sphinx: ...`), not brackets.
- Use the canonical spelling consistently -- avoid ad-hoc variants
  (`Editorial change:`, `Editorial changes:`, `GitHub actions:`, and so on).
- Issue references go in the body (`Refs #123` / `Closes #123`), not the
  subject.

The tag axis differs by area. *Content* tags name the *nature* of the change
(the area is always content); *infrastructure* tags name the *component* touched
(the verb in the description carries add/fix/update).

| Area            | Tag             | Use                                                 |
|-----------------|-----------------|-----------------------------------------------------|
| content         | `New content:`  | adding new material                                 |
| content         | `Editorial:`    | wording, style, typos, grammar, prose restructuring |
| content         | `Correction:`   | fixing an error in an example or explanation        |
| infrastructure  | `Frontend:`     | general frontend code                               |
| infrastructure  | `Widget:`       | the interactive widget                              |
| infrastructure  | `SCSS:`         | styles                                              |
| infrastructure  | `Sphinx:`       | Sphinx plugins / extensions / config                |
| infrastructure  | `Python:`       | Python tooling / modules                            |
| infrastructure  | `Test script:`  | test tooling                                        |
| infrastructure  | `CI:`           | GitHub Actions / CI                                 |
| infrastructure  | `Vagrant:`      | VM provisioning                                     |
| infrastructure  | `Makefile:`     | build pipeline                                      |
| infrastructure  | `Docs:`         | repository docs (README, CONTRIBUTING, and so on)   |

Example:

```
Sphinx: fix anchor resolution across multi-project builds

The multi-project layout broke cross-references because <...>. This restores
<...> and adds a regression check.

Refs #1361
```
