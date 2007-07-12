(defsystem "mulkutils-tests"
  :description "Unit tests for Matthias Benkard's Mulkutils."
  :author "Matthias Benkard <matthias@benkard.de>"
  :licence "GNU General Public License, version 2 or higher"
  :depends-on (:mulkutils :lift)
  :components ((:file "tests"))
  :serial t)
