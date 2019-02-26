;; -------------------------------------------------------------------------------------
;; SAMPLE APPS
;; -------------------------------------------------------------------------------------

;; Partial rewrite of https://www.javatpoint.com/notepad
(defun notepad-app ()
  (let*
      ((f
         (frame "Notepad App" 640 480))
       (status-bar
         (label "||       Ln 1, Col 1  " +align-right+))
       (ta                              ;textarea
         (textarea "" 30 60))
       (not-implemented
         (lambda (e)
           (declare (ignore e))
           (show-warning-message f "Not Implemented"))))
    ;; Here we go...
    (add-using-borderlayout f
                            :center (scrollpane ta)
                            :south status-bar
                            :east (label "   ")
                            :west (label "   "))
    (pack f)
                                        ;(#"setLocation" f 100 50)

    ;; menu bar
    (set-menu-bar f 
                  (list 
                   (menu "File" 
                         (list 
                          (menuitem "New" not-implemented)
                          (menuitem "Open" not-implemented)
                          (menuitem "Save" not-implemented)
                          (menuitem "Save as..." not-implemented)))
                   (menu "Edit" 
                         (list 
                          (menuitem "Undo!" not-implemented)
                          (separator)
                          (menuitem "Cut" not-implemented)
                          (menuitem "Copy" not-implemented)
                          (menuitem "Paste" not-implemented)
                          (menuitem "Delete" not-implemented)
                          (separator)
                          (menuitem "Select all" not-implemented)
                          ))
                   (menu "Format" ())
                   (menu "View" ())
                   (menu "Help" ())))

    ;; right-click popup menu
    (add-popupmenu-to-container
     ta                                 ;to text area 
     (popupmenu "Right click menu" 
                (list
                 (menuitem "Undo!" not-implemented)
                 (separator)
                 (menuitem "Cut")
                 (menuitem "Copy")
                 (menuitem "Paste"))))

    ;; caretlistener for text area
    (add-caretlistener ta
                       (lambda (e)
                         (declare (ignore e))
                         (let* ((pos (#"getCaretPosition" ta))
                                (line (#"getLineOfOffset" ta pos))
                                (col (- pos (#"getLineStartOffset" ta line))))
                           (#"setText" status-bar
                                       (format nil "||   Ln ~D, Col ~D"
                                               line
                                               col)))))
    (set-visible f)))


;; simple app for concatenating text files

(defun concatenate-app ()
  (handler-case 
      (let* ((f (frame "File Concatenation"))
             (list-model (defaultlistmodel))
             (l (jlist list-model)))

        ;; popup menu for list
        (add-popupmenu-to-container
         l
         (popupmenu "Menu" 
                    (list
                     (menuitem "Remove Item"
                               (lambda (e)
                                 (declare (ignore e))
                                 (let ((confirm
                                         (show-confirm-dialog f "Are you sure?")))
                                   (when (eql +dialog-yes+
                                              confirm)
                                     ;; remove item from list...
                                     ;; at index...
                                     (let ((selected-i
                                             (#"getSelectedIndex" l)))
                                       (handler-case (#"remove" list-model selected-i)
                                         (java:java-exception (x)
                                           (format t "JavaException: ~A ~%" x)))))))))))
        (add-using-borderlayout
         f
         :center (scrollpane l)             ;list inside scrollpane
         :north (label "Enter list of files:" +align-center+)
         :east (button "Add file" 
                       (lambda (e)
                         (declare (ignore e))
                         ;; add (choose) file
                         (let* ((chooser (file-chooser))
                                file)
                           ;; add extensions to chooser
                           (file-chooser-add-extension chooser "CSV file" '("csv"))
                           (file-chooser-add-extension chooser "Text file" '("txt" "text"))
                           ;; open file chooser
                           (setf file
                                 (open-file-chooser chooser f))
                           ;;add file to list
                           (when file
                             (defaultlistmodel-add list-model file)))))
         :south (button "Concatenate!"
                        (lambda (e)
                          (declare (ignore e))
                          (show-warning-message f "LOL... not Implemented!"))))
        (pack f))
    (java:java-exception (x)
      (format t "JavaException: ~A ~%" x))
    (error (x)
      (format t "Error: ~A ~%" x))))


;; -------------------------------------------------------------------------------------
;; Look and feel (test)

;; set look and feel to motif
(defun set-motif-laf (frame)
  (#"setLookAndFeel" 'UIManager "com.sun.java.swing.plaf.motif.MotifLookAndFeel")
  (#"updateComponentTreeUI" 'SwingUtilities frame)
  (#"pack" frame)) ; TODO: is pack ok here?



;; -------------------------------------------------------------------------------------
;; Trying RSyntaxTextArea

(defun rst ()
  ;; esto es un poco impredecible...
  (add-to-classpath #P"java_libs/rsyntaxtextarea-3.0.0-SNAPSHOT.jar")
  (jss:jar-import "java_libs/rsyntaxtextarea-3.0.0-SNAPSHOT.jar")
  (let* ((f (frame "RSyntaxTextArea demo" 640 480))
         (cp (jss:new 'JPanel
                      (jss:new 'BorderLayout)))
         (r (jss:new 'RSyntaxTextArea 20 60)))

    
    (#"setSyntaxEditingStyle" r
                              ;; this does not work.
                              ;; btw. the class is an Interface. 
                              ;; (java:jclass-field "org.fife.ui.rsyntaxtextarea.SyntaxConstants"
                              ;;                    "SYNTAX_STYLE_JAVA"))
                              ;; this does work.
                              (jss:get-java-field 'SyntaxConstants "SYNTAX_STYLE_LISP"))
    (#"setCodeFoldingEnabled" r +true+)
    (#"setAutoIndentEnabled" r +true+)
    (let ((sp (jss:new 'RTextScrollPane r)))
      (#"add" cp sp)
      (#"setContentPane" f cp)
      (#"pack" f)
      (#"setLocationRelativeTo" f +null+))))


;; ------------------
