
(defun get-class (class-or-name)
  "Obtain the class designed by the name, if it's a string,
otherwise return the java object"
  (etypecase class-or-name
    (string (jclass class-or-name))
    (java-object class-or-name))) ;TODO: how to specialize? 

(defun get-constructors (class-name)
  "Obtain list of constructors defined by the class itself."
  (loop with x = (get-class class-name)
        for c across (java:jclass-constructors x)
        when (equal (#"getName" (#"getDeclaringClass" c))
                    (java:jclass-name x))
        collect (to-list c)))


(defun get-methods (class-name &key (only-local T))
  "Obtain list of methods defined by the class itself.
Unless only-local = NIL, in which case gets all available methods."
  (loop with x = (get-class class-name)
        for c across (java:jclass-methods x)
        when (if only-local (equal (#"getName" (#"getDeclaringClass" c))
                                   (java:jclass-name x))
                 T)
        collect (to-list c)))


;; pretty print classes and other java objects
(defgeneric to-list (c))

(defmethod to-list ((c (java:jclass "java.lang.Class")))
  (list :name (#"getName" c)
        :interfaces (#"getInterfaces" c)
        :classes (#"getClasses" c)
        :genericsuperclass (#"getGenericSuperclass" c)
        :methods (get-methods c)
        :constructors (get-methods c)
        ))

(defmethod to-list ((c (java:jclass "java.lang.reflect.Method")))
  (list :name (#"getName" c)
        :parameter-types (#"getParameterTypes" c)
        :return-type (#"getReturnType" c)
        :parameters (#"getParameters" c)))

(defmethod to-list ((c (java:jclass "java.lang.reflect.Constructor")))
  (list :parameter-types (#"getParameterTypes" c)
        :parameters (#"getParameters" c)))

;; list to swing List
;; can only show String values...
(defun k-v-to-string (param value)
  (format nil "~40A: ~80A" param value))

(defun plist-to-listmodel (plist)
  (let  ((lm (defaultlistmodel)))
    (alexandria:doplist (key val plist)
                        (defaultlistmodel-add lm (k-v-to-string key val)))
    lm))

(defmacro list-listener (list jl f)
  `(listselectionlistener (lambda (e)
                           (declare (ignore e))
                           (let* ((selected-i
                                    (#"getSelectedIndex" ,jl))
                                  (selected-list
                                    (elt ,list selected-i)))
                             ;; show inspector for list or plist or etc.
                             (cond
                               ((typep selected-list 'cons)
                                (if (keywordp (car selected-list))
                                    (display-plist selected-list)
                                    (display-list selected-list)))
                               ((typep selected-list 'string)
                                (show-message-dialog
                                 ,f
                                 (format nil  "String: ~A" selected-list)))
                               (t
                                (show-warning-message ,f "Can't inspect!")))
                             ))))

(defun display-list (list)
  (let* ((lm (defaultlistmodel))
         (f (frame "Inspecting list" 640 480))
         (jl (jlist lm)))
    (#"addListSelectionListener"
     jl
     (list-listener list jl f))
    (loop for item in list
         do (defaultlistmodel-add lm (format nil "~80A" item)))
    (add-using-borderlayout f
                            :center (scrollpane jl))))

(defun plist-to-jarray (plist)
  "Convert PLIST into jarray of jarray<string>"
  (let (x)
    (alexandria:doplist (key val plist)
                        (push
                         (list (format nil "~A" key)
                               (format nil "~A" val))
                         x))
    (java:jnew-array-from-list "java.lang.String" (nreverse x))))

(defun display-plist (plist)
  "As JTable"
  (let* ((dtm (defaulttablemodel '("Key" "Value")))
         (jt (jtable dtm))
         (f (frame "Inspecting plist" 640 480)))
    ;; add data...
    (alexandria:doplist
        (key val plist)
        (#"addRow" dtm (list-to-jarray (list
                                        (format nil "~A" key)
                                        (format nil "~A" val)))))
    (add-using-borderlayout f
                            :center (scrollpane jt))))

(defun display-table (list &key (title ""))
  "Display table (list of plists with same length)"
  (let ((column-names nil)
        ;(width nil)
        )
    (alexandria:doplist (k v (car list))
                        (push (format nil  "~10A" k) column-names))
    (nreverse column-names)
    ;(setf width (length column-names))
    (let* ((dtm (defaulttablemodel column-names))
           (jt (jtable dtm))
           (f (frame title 640 480)))
      (loop for row in list ;for each row
            do
            (let ((row2))
              (alexandria:doplist (k v row)
                                  (push (format nil "~10A" v) row2))
              (nreverse row2)
              (#"addRow" dtm (list-to-jarray row2))))
      (add-using-borderlayout f
                              :center (scrollpane jt)))))

(defun display-methods (class-name &optional (only-local T))
  (display-table (get-methods class-name :only-local only-local)
                 :title (format nil "Methods of ~A" class-name)
                 ))

