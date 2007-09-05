(defsystem "mulkutils"
  :description "Random utilities by Matthias Benkard."
  :version "0.3"
  :author "Matthias Benkard <matthias@benkard.de>"
  :licence "GNU General Public License, version 2 or higher"
  :depends-on (:arnesi :unification)
  :components ((:file "package")
               (:file "lambda")
               (:file "unification"))
  :serial t)
