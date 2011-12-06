(define-skeleton cpp-filewrite-ske
  "Insert a binary file writing expression."
  "promp: "
  >"FileOut.write((char*)&" _ ", " str ");\n"
)

(define-skeleton fvwm-new-menu-ske
  "Insert a new menu element."
  "Name of new menu: "
  "DestroyMenu " str "\n"
  "AddToMenu " str "\n"
  "+ \"" _ "%%\""
)

(define-skeleton fvwm-new-function-ske
  "Insert a new menu element."
  "Name of new menu: "
  "DestroyFunc " str "\n"
  "AddToFunc " str "\n"
  "+ I "_
)
