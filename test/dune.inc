(rule (targets 001.feed) (mode fallback) (deps (:gen downloader.ml)) (action (run %{ocaml} %{gen} --uri http://16andcounting.libsyn.com/rss --output %{targets})))
(rule (targets 002.feed) (mode fallback) (deps (:gen downloader.ml)) (action (run %{ocaml} %{gen} --uri http://rgrinberg.com/blog/atom.xml --output %{targets})))
(rule (targets 003.feed) (mode fallback) (deps (:gen downloader.ml)) (action (run %{ocaml} %{gen} --uri http://ocaml.org/feed.xml --output %{targets})))
(rule (targets 004.feed) (mode fallback) (deps (:gen downloader.ml)) (action (run %{ocaml} %{gen} --uri http://korben.info/feed --output %{targets})))
(rule (targets 005.feed) (mode fallback) (deps (:gen downloader.ml)) (action (run %{ocaml} %{gen} --uri http://linuxfr.org/journaux.atom --output %{targets})))
(rule (targets 006.feed) (mode fallback) (deps (:gen downloader.ml)) (action (run %{ocaml} %{gen} --uri http://www.reddit.com/r/ocaml/.rss --output %{targets})))

(alias (name runtest) (package syndic) (deps (:test test.exe) (:feeds 001.feed 002.feed 003.feed 004.feed 005.feed 006.feed) feeds.json) (action (run %{test} --color=always)))

