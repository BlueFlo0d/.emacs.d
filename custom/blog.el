(setq org-static-blog-publish-title "Q. Hong's Personal Blog")
(setq org-static-blog-publish-url "/")
(setq org-static-blog-publish-directory "~/blog/")
(setq org-static-blog-posts-directory "~/blog/posts/")
(setq org-static-blog-drafts-directory "~/blog/drafts/")
(setq org-static-blog-enable-tags t)
(setq org-export-with-toc nil)
(setq org-export-with-section-numbers nil)

;; This header is inserted into the <head> section of every page:
;;   (you will need to create the style sheet at
;;    ~/projects/blog/static/style.css
;;    and the favicon at
;;    ~/projects/blog/static/favicon.ico)
(setq org-static-blog-page-header
"<meta name=\"author\" content=\"Q. Hong\">
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
<link href= \"static/style.css\" rel=\"stylesheet\" type=\"text/css\" />
<link rel=\"icon\" href=\"static/favicon.ico\">")

;; This preamble is inserted at the beginning of the <body> of every page:
;;   This particular HTML creates a <div> with a simple linked headline
(setq org-static-blog-page-preamble
"<div class=\"header row\">
  <div class=\"lcol\"><a href=\"/\">Q. Hong's Personal Blog</a></div>
</div><hr/>")

(defun org-static-blog-post-postamble (post-filename)
  "Returns the tag list of the post.
This function is called for every post and appended to the post body.
Modify this function if you want to change a posts footline."
  (let ((taglist-content ""))
    (when (and (org-static-blog-get-tags post-filename) org-static-blog-enable-tags)
      (setq taglist-content (concat "<div class=\"taglist\">"
                                    "<a href=\""
                                    (org-static-blog-get-absolute-url org-static-blog-tags-file)
                                    "\">" (org-static-blog-gettext 'tags) "</a>: "))
      (dolist (tag (org-static-blog-get-tags post-filename))
        (setq taglist-content (concat taglist-content "<a href=\""
                                      (org-static-blog-get-absolute-url (concat "tag-" (downcase tag) ".html"))
                                      "\">" tag "</a> ")))
      (setq taglist-content (concat taglist-content "</div><hr/>")))
    taglist-content))

;; This postamble is inserted at the end of the <body> of every page:
;;   This particular HTML creates a <div> with a link to the archive page
;;   and a licensing stub.
(setq org-static-blog-use-preview t)
(setq org-static-blog-page-postamble
"<h2>Comments</h2>
<!--%comments%-->
<form action=\"?\" method=\"POST\"><table>
<tr><td class=\"ltd\"><label for=\"nickname\">Nickname (*): </label></td><td><input type=\"text\" name=\"nickname\" id=\"nickname\"/></td></tr>
<tr><td class=\"ltd\"><label for=\"contact\">Email: </label></td><td><input type=\"text\" name=\"contact\" id=\"contact\"/></td></tr>
<tr><td class=\"ltd\"><label for=\"rep\">Reply to #:</label></td><td><input type=\"text\" name=\"rep\" id=\"rep\"/></td></tr>
<tr><td class=\"ltd\"><label for=\"text\">Text (*):</label></td><td><textarea name=\"text\" id=\"text\" rows=\"10\"></textarea></td></tr>
<tr><td class=\"ltd\"><label for=\"url\">or URL (*):</label></td><td><input type=\"text\" id=\"url\" name=\"url\"/></td></tr>
<tr><td class=\"ltd\"><label for=\"cap\">CAPTCHA:
<!--%captcha%-->
</label></td><td><input type=\"text\" id=\"cap\" name=\"cap\"/></td></tr>
</table>
<table style=\"table-layout: fixed;\"><tr><td><input type=\"submit\"/></td><td><input type=\"reset\"/></td></tr>
</table></form>
<hr/><div id=\"archive\">
  <a href=\"/archive.html\">Other posts</a>
</div>
<center><a rel=\"license\" href=\"https://creativecommons.org/licenses/by-sa/3.0/\"><img alt=\"Creative Commons License\" style=\"border-width:0\" src=\"https://i.creativecommons.org/l/by-sa/3.0/88x31.png\" /></a><br /><span xmlns:dct=\"https://purl.org/dc/terms/\" href=\"https://purl.org/dc/dcmitype/Text\" property=\"dct:title\" rel=\"dct:type\">cat-v.mit.edu</span> by Q. Hong is licensed under a <a rel=\"license\" href=\"https://creativecommons.org/licenses/by-sa/3.0/\">Creative Commons Attribution-ShareAlike 3.0 Unported License</a>.</center>")

(provide 'blog)
