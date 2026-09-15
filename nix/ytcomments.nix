{ pkgs }:

pkgs.writeShellApplication {
  name = "ytcomments";
  runtimeInputs = [
    pkgs.python3
    pkgs.yt-dlp
  ];
  text = ''
    set -euo pipefail

    if [ "$#" -lt 1 ]; then
      printf 'Usage: ytcomments URL\n' >&2
      exit 2
    fi

    tmp_dir=$(mktemp -d)
    trap 'rm -rf "$tmp_dir"' EXIT

    yt-dlp \
      --quiet \
      --no-warnings \
      --skip-download \
      --write-comments \
      --dump-single-json \
      --extractor-args 'youtube:comment_sort=top' \
      "$1" > "$tmp_dir/comments.json"

    python3 - "$tmp_dir/comments.json" <<'PY'
    import curses
    import json
    import sys
    import textwrap


    def load_comments(path):
        with open(path, encoding="utf-8") as stream:
            comments = json.load(stream).get("comments") or []

        children = {}
        for comment in comments:
            parent = comment.get("parent") or "root"
            children.setdefault(parent, []).append(comment)

        return children


    def visible_comments(children, expanded):
        result = []

        def visit(parent, depth):
            siblings = children.get(parent, [])

            for index, comment in enumerate(siblings):
                result.append((
                    comment,
                    depth,
                    index == len(siblings) - 1,
                ))

                if comment.get("id") in expanded:
                    visit(comment.get("id"), depth + 1)

        visit("root", 0)
        return result


    def add_text(window, row, column, text, attributes=0, width=None):
        height, screen_width = window.getmaxyx()

        if row < 0 or row >= height or column >= screen_width:
            return

        text = text.replace("\t", "    ")

        if width is not None:
            text = text[:max(0, width)]

        try:
            window.addnstr(
                row,
                column,
                text,
                max(0, screen_width - column - 1),
                attributes,
            )
        except curses.error:
            pass


    def run(stdscr, children):
        curses.curs_set(0)
        stdscr.keypad(True)

        expanded = set()
        selected = 0
        scroll = 0

        while True:
            rows = visible_comments(children, expanded)
            selected = max(0, min(selected, max(0, len(rows) - 1)))

            height, width = stdscr.getmaxyx()
            stdscr.erase()

            split_view = width >= 90
            tree_width = width // 2 if split_view else width
            detail_top = 0 if split_view else max(3, height // 2)
            tree_height = height if split_view else detail_top
            visible_height = max(1, tree_height - 2)

            if selected < scroll:
                scroll = selected

            if selected >= scroll + visible_height:
                scroll = selected - visible_height + 1

            scroll = max(0, scroll)

            add_text(
                stdscr,
                0,
                0,
                "Comments  j/k: move  h/l: parent/child  f: fold  q: quit",
                curses.A_BOLD,
                width,
            )

            if split_view:
                try:
                    stdscr.vline(
                        1,
                        tree_width,
                        curses.ACS_VLINE,
                        max(0, height - 1),
                    )
                except curses.error:
                    pass

            for screen_row, row_index in enumerate(
                range(scroll, min(len(rows), scroll + visible_height)),
                1,
            ):
                comment, depth, is_last = rows[row_index]
                author = comment.get("author") or "Unknown"
                likes = comment.get("like_count") or 0
                branch = "└── " if is_last else "├── "

                line = (
                    ("  " * depth)
                    + branch
                    + f"{author} [{likes} likes]"
                )

                attributes = curses.A_REVERSE if row_index == selected else 0

                add_text(
                    stdscr,
                    screen_row,
                    0,
                    line,
                    attributes,
                    tree_width - 1,
                )

            if rows:
                comment = rows[selected][0]
                detail_width = (
                    width - tree_width - 3
                    if split_view
                    else width - 2
                )
                detail_x = tree_width + 2 if split_view else 1
                detail_y = 1 if split_view else detail_top + 1
                detail_height = height - detail_y

                author = comment.get("author") or "Unknown"
                likes = comment.get("like_count") or 0

                add_text(
                    stdscr,
                    detail_y,
                    detail_x,
                    f"{author}  |  {likes} likes",
                    curses.A_BOLD,
                    detail_width,
                )

                text = comment.get("text") or "(no text)"
                lines = []

                for paragraph in text.splitlines() or [""]:
                    lines.extend(
                        textwrap.wrap(
                            paragraph,
                            max(1, detail_width),
                        ) or [""]
                    )

                for offset, line in enumerate(
                    lines[:max(0, detail_height - 2)],
                    1,
                ):
                    add_text(
                        stdscr,
                        detail_y + offset,
                        detail_x,
                        line,
                        width=detail_width,
                    )

            stdscr.refresh()
            key = stdscr.getch()

            if key in (ord("q"), 27):
                return

            if key in (ord("j"), curses.KEY_DOWN):
                selected = min(selected + 1, max(0, len(rows) - 1))

            elif key in (ord("k"), curses.KEY_UP):
                selected = max(0, selected - 1)

            elif key in (curses.KEY_NPAGE, 4):
                selected = min(
                    selected + max(1, tree_height // 2),
                    max(0, len(rows) - 1),
                )

            elif key in (curses.KEY_PPAGE, 21):
                selected = max(0, selected - max(1, tree_height // 2))

            elif key == ord("f") and rows:
                comment_id = rows[selected][0].get("id")

                if children.get(comment_id):
                    if comment_id in expanded:
                        expanded.remove(comment_id)
                    else:
                        expanded.add(comment_id)

            elif key == ord("l") and rows:
                comment_id = rows[selected][0].get("id")

                if children.get(comment_id):
                    expanded.add(comment_id)
                    selected = min(selected + 1, len(rows))

            elif key == ord("h") and rows:
                parent = rows[selected][0].get("parent") or "root"

                if parent != "root":
                    parent_ids = [row[0].get("id") for row in rows]

                    if parent in parent_ids:
                        selected = parent_ids.index(parent)


    def main(path):
        children = load_comments(path)
        curses.wrapper(run, children)


    if __name__ == "__main__":
        main(sys.argv[1])
    PY
    '';
}
