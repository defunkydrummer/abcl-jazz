
(defun concatenar (files file-out parent-frame)
  "Concatenar todos los files (asumimos de texto)"
  (handler-case
      (progn
        ;; mostrar frame de alerta
        (let ((fr (frame "Progreso" 640 480))
              (text (textarea "" 10 80)))
          (add-using-borderlayout
           fr
           :north (label "Archivos en proceso:")
           :center (scrollpane text))
          (with-open-file (str-out file-out :direction :output)
            (loop for f in files
                  do
                  ;; append to text area
                  (#"append" text
                             (format nil "File: ~A ~%" f))
                  ;; (show-message-dialog parent-frame
                  ;;                      (format nil "File: ~A" f))
                  (with-open-file (str-in f :direction :input)
                    (loop for line = (read-line str-in nil 'EOF)
                          while (not (equal line'EOF))
                          do
                          (write-line line str-out)))))
          (show-message-dialog fr "Proceso completado.")
          ;(set-visible fr +false+)
          ))
    (java:java-exception (e)
      (show-message-dialog parent-frame (format nil "Java Exception: ~A" e)))
    (error (e)
      (show-message-dialog parent-frame (format nil "Error: ~A" e)))))

(defun vista-previa (path)
  "Popup de vista previa de archivo."
  (let* ((f (frame (format nil "Vista previa ~A" path) 640 480))
         (text (textarea "" 40 80)))
    (textarea-set-font text (font-fixed 20))
    (with-open-file (str-in path :direction :input)
      (loop with cnt = 0
            for line = (read-line str-in nil 'EOF)
            while (and  (< cnt 100)
                        (not (equal line'EOF)))
            do
            (#"append" text (format nil "~2d:~80A~%" cnt line))
            (incf cnt))
    (add-using-borderlayout f
                            :center (scrollpane text)))))


(defun concatenate-app ()
  (handler-case 
      (let* ((f (frame "Concatenar archivos" 800 600))
             (list-model (defaultlistmodel))
             (l (jlist list-model)))
        (flet ((remover-de-lista (e)
                 (declare (ignore e))
                 (let ((confirm
                         (show-confirm-dialog f
                                              (format nil "Eliminar item?" ))))
                   (when (eql +dialog-yes+ confirm)
                     (let ((selected-i
                             (#"getSelectedIndex" l)))
                       (#"remove" list-model selected-i)))))
               (vista (e)
                 (declare (ignore e))
                 ;; vista previa del file
                 (let ((path (defaultlistmodel-get-element-at
                              list-model
                              (#"getSelectedIndex" l))))
                   (vista-previa path))
                 ))
          ;; popup menu for list
          (add-popupmenu-to-container l
                                      (popupmenu "Menu" 
                                                 (list
                                                  (menuitem "Eliminar item" #'remover-de-lista)
                                                  (menuitem "Vista Previa" #'vista))))
          (add-using-borderlayout
           f
           :center (scrollpane l)       ;list inside scrollpane
           :north (label "Ingrese lista de archivos:" +align-center+)
           :east
           (let ((p (panel (gridlayout 8 1))))
             (add-to p
                     (list
                      (button "Agregar" 
                              (lambda (e)
                                (declare (ignore e))
                                ;; add (choose) file
                                (let* ((chooser (file-chooser))
                                       files)
                                  ;; add extensions to chooser
                                  (file-chooser-add-extension chooser "Archivo CSV" '("csv"))
                                  (file-chooser-add-extension chooser "Archivo de texto" '("txt" "text"))
                                  ;; open file chooser
                                  (setf files (open-file-chooser-multiple-selection chooser f))
                                  ;;add files to list
                                  (when files
                                    (loop for f in files do
                                          (defaultlistmodel-add list-model
                                                                (getf f :path)))))))
                      (button "Eliminar de la lista" #'remover-de-lista)
                      (button "Vista Previa" #'vista)
                      (button "Concatenar!"
                              (lambda (e)
                                (declare (ignore e))
                                ;; imprimir items...
                                (format t "Elements: ~A"
                                        (defaultlistmodel-getlist list-model))
                                (force-output)
                                
                                (let* ((chooser (file-chooser))
                                       (file (open-file-chooser-save chooser f)))
                                  ;; guardar
                                  (if file
                                      (progn
                                        ;; concatenar todo
                                        (concatenar
                                         (defaultlistmodel-getlist list-model)
                                         (getf file :path)
                                         f)
                                        ))
                                  
                                  )))
                      ))
             p)
           :south
           ;; fila de botones
           (let ((p (panel (flowlayout +flow-layout-leading+
                                       ))))
             ;; agregar al panel
             (add-to p
                     (list
                      
                      ;; codificacion
                      (label "Codificacion de entrada:" +align-left+)
                      (jcombobox '("utf8" "iso-8859-1" "utf32"))
                      (label "Codificacion de salida:" +align-left+)
                      (jcombobox '("utf8" "iso-8859-1" "utf32"))
                      )
                      )
                     p))
          (pack f)))
    (java:java-exception (x)
      (format t "JavaException: ~A ~%" x))
    (error (x)
      (format t "Error: ~A ~%" x))))
