;; (require :abcl-contrib)
;; (require :jss)
;; (setf jss:*muffle-warnings* nil)

;; ;; Follows the following SWING tutorial:
;; ;; https://docs.oracle.com/javase/tutorial/uiswing/lookandfeel/plaf.html#programmatic


;; easy action listener creator

(defun handle-java-error (e)
  "Stub for handling a JavaException thrown within the Swing UI"
  (format t "~40a~%~40a~%" "handle-java-error:" e)
  (force-output)) ;important for showing the message ASAP


(defun actionlistener (handler-function)
  "Easy creation of action listener, handler function requires to be provided.
Function shall take one parameter -- event (the event)"
  ;; instantiate proxy that implements actionListener
  (java:jinterface-implementation 
   "java.awt.event.ActionListener" 
   "actionPerformed"
   ;; wrap the handler function
   (lambda (e)
     (handler-case
         (funcall handler-function e)
       (java:java-exception (x)
         (handle-java-error x))))))



;; helper to test if it works
(defun %test-actionlistener (listener)
  (#"actionPerformed" listener +null+))

;; easy button creator
(defun button (text &optional handler-function)
  "Create a button (with text).
Handler-function is a function that works as the action listener for the button."
  (let ((b (jss:new 'JButton text)))
    (when handler-function
      (#"addActionListener" b (actionlistener handler-function)))
    b))


(defun pack (component)
  "Apply Pack method to component."
  (#"pack" component))

(defun set-visible (component &optional (visible? +true+) )
  "Set component visible (or not). Use Java booleans +true+ + +false+!!"
  (#"setVisible" component visible?))

(defun set-size (component size-x size-y)
  "Set size to component."
  (#"setSize" component size-x size-y))

;; frame creator
(defun frame (title &optional size-x size-y)
  "Create a new frame. And make it visible."
  (let ((f (jss:new 'JFrame title)))
    (when (and size-x size-y)
      (set-size f size-x size-y))
    (set-visible f)
    f))


(defmacro %add-to-component (component-var items-var)
  "Utility macro. Adds to component/container."
  `(loop for i in ,items-var
        do
         (#"add" ,component-var i)))

(defun add-to (container items)
  "Add the list of components (items) to the container."
  (%add-to-component container items))

(defun add-with-constraints (container list)
  "Add the list of components (items in alist) to the container.
Each item of the list should be a CONS or list where the
CAR is the component to be added, the (optional) CDR should be 
the constraints specified (i.e. a string)."
  (loop for i in list do
        (if (cdr i) (#"add" container (car i) (cdr i))
            (#"add" container (car i)))))

;; add things to frame
;; TODO: rewrite as macro.
;; (defun frame-add (frame items &key (pack T))
;;   "Add items to the JFrame frame."
;;   (%add-to-component frame items)
;;   (when pack
;;     (pack frame)))

;; JPanel

(defun panel (&optional layout-manager)
  "Create a new JPanel."
  (if layout-manager
      (jss:new 'JPanel layout-manager)
      (jss:new 'JPanel)))

;; (defun panel-add (panel items)
;;   "Add items to the JPanel panel."
;;   (%add-to-component panel items))

;; JTabbedPane

(defun tabbedpane ()
  "JTabbedPane constructor."
  (jss:new 'JTabbedPane))

;; (defun tabbedpane-add (tp items)
;;   "Add items to tabbedpane"
;;   (%add-to-component tp items))

;; JDialog - modeless dialog
(defun dialog (&optional parent-frame (title "") (is-modal +false+))
  "Create a JDialog."
  (if parent-frame
      (jss:new 'JDialog parent-frame title is-modal)
      (jss:new 'JDialog)))


;;  horizontal alignment from SwingConstants
(defparameter +align-right+ (jss:get-java-field 'SwingConstants "RIGHT"))
(defparameter +align-left+ (jss:get-java-field 'SwingConstants "LEFT"))
(defparameter +align-center+ (jss:get-java-field 'SwingConstants "CENTER"))
;; and more
(defparameter +align-top+ (jss:get-java-field 'SwingConstants "TOP"))
(defparameter +align-bottom+ (jss:get-java-field 'SwingConstants "BOTTOM"))
(defparameter +align-leading+ (jss:get-java-field 'SwingConstants "LEADING"))
(defparameter +align-trailing+ (jss:get-java-field 'SwingConstants "TRAILING"))


(defun label (text &optional horizontal-alignment)
  "Create a new JLabel."
  (let ((l (jss:new 'JLabel text)))
    (when horizontal-alignment
      (#"setHorizontalAlignment" l horizontal-alignment))
    l))

(defun textfield (&optional (text "")
                            columns
                            (editable +true+))
  "Create a new JTextField"
  (let ((tx
          (if columns
               (jss:new 'JTextField text columns)
               (jss:new 'JTextField text))))
    (#"setEditable" tx editable)
    tx))

(defun textarea (&optional (text "")
                           rows
                           columns)
  "Create a JTextarea."
  (let ((ta
          (jss:new 'JTextArea text)))
    (when rows
      (#"setRows" ta rows))
    (when columns
      (#"setColumns" ta columns))
    ta))

;; text area font example
(defun font-fixed (size)
  "Get fixed spacing font of required size"
  (jss:new "java.awt.Font" "Courier New" 
           (jss:get-java-field 'java.awt.Font "PLAIN") size))

(defun textarea-set-font (tx font)
  (#"setFont" tx font))

(defun add-caretlistener (text-component caret-update-function)
  "Create and add a CaretListener for a text component, 
which calls caret-update-function (sending event e) on change in the caret position of a text component."
  (#"addCaretListener" text-component 
                       ;; caretListener implementation
                       (java:jinterface-implementation
                        "javax.swing.event.CaretListener"
                        "caretUpdate"
                        ;; impl for caretUpdate
                        caret-update-function)))

(defun scrollpane (control)
  "Create scrollpane for supplied control"
  (jss:new 'JScrollPane control))


;; TODO: JPasswordField
;; TODO: JCheckBox
;; TODO: JCheckBox : ItemListener
;; TODO: JRadioButton



(defun listselectionlistener (handler-function)
  "Easy creation of ListSelectionListener, handler function requires to be provided.
Function shall take one parameter -- event (ListSelectionListener event)"
  ;; instantiate proxy that implements actionListener
  (java:jinterface-implementation
   "javax.swing.event.ListSelectionListener" 
   "valueChanged"
   handler-function))

(defun defaultlistmodel (&optional elements)
  "Creates a new DefaultListModel. Optionally, adds elements from the supplied list.."
  (let ((l
          (jss:new 'DefaultListModel)))
    (when elements
      (loop for i in elements
            do
            (#"addElement" l i)))
    l))

(defun defaultlistmodel-add (list-model element)
  "Add element to listmodel."
  (#"addElement" list-model element))

(defun defaultlistmodel-get-element-at (list-model index)
  "Obtain element at index. (Index is zero-based)"
  (#"elementAt" list-model index))

(defun defaultlistmodel-insert-element-at (list-model element index)
  "Insert element at index. (Index is zero-based)"
  (#"insertElementAt" list-model element index))

(defun defaultlistmodel-remove-element-at (list-model index)
  "Remove element at index and return it. (Index is zero-based)"
  (#"remove" list-model index))

;; (defun defaultlistmodel-move-element (list-model index offset)
;;   "Move up or down element at index. (Index is zero-based)
;; Offest: -1 = move up, +1 = move down"
;;   (let ((new-index (+ index offset))
;;         (elem (defaultlistmodel-get-element-at list-model index))
;;         (all (defaultlistmodel-getlist list-model)))
;;     (when (and elem
;;                (>= new-index 0)
;;                (< new-index (defaultlistmodel-size list-model)))
;;       ;; clear list, create it again...
;;       (#"clear" list-model)
;;       (loop for x from 0 to (1- (length all))
;;             do
;;             (cond
;;               ((equal x new-index)
;;                (#"addElement" list-model elem))
;;               ((equal x index) nil)
;;               (t (#"addElement" list-model (elt all index)))))
;;       (defaultlistmodel-insert-element-at list-model elem new-index)
;;       (defaultlistmodel-remove-element-at list-model index))))


(defun defaultlistmodel-size (list-model)
  "Obtain number of elements"
  (#"getSize" list-model))

(defun defaultlistmodel-getlist (list-model)
  "Get all elements"
  (loop for x from 0 to (1- (defaultlistmodel-size list-model))
        collecting (defaultlistmodel-get-element-at list-model x)))

(defun jlist (&optional list-model selection-listener-function)
  "Creats a new JList. Optionally uses a list handler function,
which shall receive one parameter, the event (ListSelectionListener event)"
  (let ((l (if list-model (jss:new 'JList list-model)
               (jss:new 'JList))))
    (when selection-listener-function
      (#"addListSelectionListener" l (listselectionlistener selection-listener-function)))
    l))


;; JScrollBar??

;;DIALOGS
;; constants for dialogs
(defun %option (str)
  (jss:get-java-field 'JOptionPane str))

;; Return values for dialogs.
(defparameter +dialog-yes+ (%option "YES_OPTION"))
(defparameter +dialog-no+ (%option "NO_OPTION"))
(defparameter +dialog-cancel+ (%option "CANCEL_OPTION"))
(defparameter +dialog-ok+ (%option "OK_OPTION"))
(defparameter +dialog-closed+ (%option "CLOSED_OPTION"))

(defun show-message-dialog (parent-component text)
  "Shows a message dialog."
  (java:jstatic "showMessageDialog"
                "javax.swing.JOptionPane"
                parent-component
                text))

(defun show-confirm-dialog (parent-component text)
  "Shows a confirm dialog."
  (java:jstatic "showConfirmDialog"
                "javax.swing.JOptionPane"
                parent-component
                text))

(defun show-warning-message (parent-component text &optional (dialog-title "Warning"))
  "Shows a warning message dialog."
  (java:jstatic "showMessageDialog"
                "javax.swing.JOptionPane"
                parent-component
                text
                dialog-title
                (jss:get-java-field 'JOptionPane "WARNING_MESSAGE")))

(defun show-input-dialog (parent-component message)
  (java:jstatic "showInputDialog"
                "javax.swing.JOptionPane"
                parent-component
                message))

;; MENU ITEMS !

(defun menu (title menu-items)
  "Creates JMenu object. Allows a list of submenus (menu items)."
  (let ((m (jss:new 'JMenu title)))
    (%add-to-component m menu-items)
    m))

(defun menuitem (title &optional handler-function)
  "Creates a menu item with its corresponding handler function."
  (let ((m (jss:new 'JMenuItem title)))
    (when handler-function
      (#"addActionListener" m (actionlistener handler-function)))
    m))

(defun set-menu-bar (frame menu-list)
  "Create a menu bar for the supplied menu list.
Then set the frame to that menu bar."
  (let ((mb (jss:new 'JMenuBar)))
    ;; add items in menu-list to mb
    (%add-to-component mb menu-list)
    ;; set this menubar on the supplied frame
    (#"setJMenuBar" frame mb)))

(defun %menu-test ()
  (let ((f (frame "Menu test!!")))
    (set-menu-bar f 
                  (list 
                   (menu "Menu1" 
                         (list 
                          (menuitem "MenuItem1" 
                                    (lambda (e)
                                      (declare (ignore e))
                                      (show-warning-message f 
                                                            "OYE NO ME APRIETES!!" "Alertita")))
                          (menuitem "Aprietame" 
                                    (lambda (e)
                                      (declare (ignore e))
                                      (show-warning-message f 
                                                            "ESO, ESO, ESO" "Alertita")))))
                   (menu "Misc" 
                         (list 
                          (menuitem "About" 
                                    (lambda (e)
                                      (declare (ignore e))
                                      (show-message-dialog f "(c) 2018 Defunkydrummer.")))
                          ))))
    ;;(set-motif-laf f)
    ))

;; Popup Menu
(defun popupmenu (title menu-items)
  "Creates JPopupMenu object. Allows a list of submenus (menu items)."
  (let ((m (jss:new 'JPopupMenu title)))
    (%add-to-component m menu-items)
    m))

;; add menu to containter
(defun add-popupmenu-to-container (container popup-menu)
  "Add the popup-menu to the frame or container.
If right-clicked, the popup menu will appear...
NOTE: This will set/reset the mouseClicked event handler on the frame."
  ;;TODO: Must be RIGHT click.
  (#"addMouseListener"
   container
   (java:jinterface-implementation
    "java.awt.event.MouseListener"
    ;; implementation of method... 
    "mouseClicked"
    ;; handler for right-click.
    (lambda (e)
      (if (equal (#"getButton" e)
                 (jss:get-java-field 'java.awt.event.MouseEvent "BUTTON3"))
          ;; show menu at point.
          (#"show" popup-menu
                   container
                   (#"getX" e)
                   (#"getY" e)))))))

(defun %popupmenu-test ()
  (let* ((f (frame "Hello World" 640 480))
         (p (popupmenu "Menu" 
                       (list (menuitem "HELLO"
                                       (lambda (e)
                                         (declare (ignore e))
                                         (show-message-dialog f "Hello my friends!")))
                             (menuitem "GOODBYE")))))
    (add-popupmenu-to-container f p)))


;; TODO: JCheckBoxMenuItem (bueno, es para agregar checkboxes en menus, bien especializado no?)


(defun separator (&optional orientation)
  "JSeparator"
  (declare (type (or fixnum null) orientation))
  (if orientation
      (jss:new 'JSeparator orientation)
      (jss:new 'JSeparator)))

;; ------------------------------------------------------------------------
;; JTABLE
;; ------------------------------------------------------------------------


;; helper
(defun list-to-jarray (list)
  "Convert list to java array if necessary."
  (cond 
        ((consp list) (java:jarray-from-list list))
        ((java:java-object-p list) list)
        (t (error "Can't conver this list."))))

(defun defaulttablemodel (column-names)
  "Create DefaultTableModel, column names not optional..."
  (let ((cols (list-to-jarray column-names)))
    (jss:new 'DefaultTableModel
             (java:jnew-array "java.lang.String" 0 0) ;data is Object[][]
             cols))) ;String[] columns

(defun jtable (table-model)
  "Create table based on DefaultTableModel"
  (jss:new 'JTable table-model))


;; ------------------------------------------------------------------------
;; COMBO BOX
;; ------------------------------------------------------------------------

(defun jcombobox (items)
  "Create a JComboBox with the following items (list of strings)"
  (jss:new 'JComboBox
           (list-to-jarray items)))

(defun jcombobox-selecteditem (cb)
  "Get the selected item of the combo box."
  (#"getSelectedItem" cb))


;; TODO: JProgressbar?
;; TODO: JTree?
;; and other other componnents.
;; TODO: JEditorPane

;; File chooser
(defun file-chooser ()
  "Create a JFileChooser"
  (jss:new 'JFileChooser))

(defun file-chooser-add-extension (chooser description extensions)
  "Add the supplied extension(s) (and description) to what the file chooser shall accept."
  (declare (type string description)
           (type cons extensions))
  (let ((filter (jss:new 'FileNameExtensionFilter
                         description
                         ;; lisp list to java array.
                         (java:jarray-from-list extensions))))
    (#"addChoosableFileFilter" chooser filter)))

(defun get-file-info (file)
  "Obtain info from a Java.io.File object as PLIST"
  (list :absolute-file (#"getAbsoluteFile" file) ;Java.io.File
        :path (#"getAbsolutePath" file)
        :name (#"getName" file)
        :parent (#"getParent" file)))

(defun open-file-chooser (chooser parent-component)
  "Open file chooser.
If file was approved, return file properties as plist."
  (let ((retval (#"showOpenDialog" chooser parent-component)))
    (if (equal retval
               (jss:get-java-field 'JFileChooser "APPROVE_OPTION"))
        ;; return the chosen filename
        (let ((file (#"getSelectedFile" chooser)))
          (get-file-info file)))))

(defun open-file-chooser-save (chooser parent-component)
  "Open file chooser for SAVE.
If file was approved, return file properties as plist."
  (let ((retval (#"showSaveDialog" chooser parent-component)))
    (if (equal retval
               (jss:get-java-field 'JFileChooser "APPROVE_OPTION"))
        ;; return the chosen filename
        (let ((file (#"getSelectedFile" chooser)))
          (get-file-info file)))))

(defun open-file-chooser-multiple-selection (chooser parent-component)
  "Open file chooser using the selected extensions (list of strings).
If file was approved, return list of file paths."
  (#"setMultiSelectionEnabled" chooser +true+)
  (let ((retval (#"showOpenDialog" chooser parent-component)))
    (if (equal retval
               (jss:get-java-field 'JFileChooser "APPROVE_OPTION"))
        ;; return the chosen filenames (ALL)
        
        (let ((files (#"getSelectedFiles" chooser)))
          (loop for f across files
                collecting (get-file-info f))))))
       


;; -------------------------------------------------------------------------------------
;; LAYOUTS
;; -------------------------------------------------------------------------------------

(defun add-using-borderlayout (parent-container
                               &key north south east west center)
  "Add components to container using BorderLayout. Each one is added in the direction specified.
I.e. :north xx adds xx in the NORTH direction.
You don't need to specify all directions."
  (let ((n (jss:get-java-field 'BorderLayout "NORTH"))
        (s (jss:get-java-field 'BorderLayout "SOUTH"))
        (e (jss:get-java-field 'BorderLayout "EAST"))
        (w (jss:get-java-field 'BorderLayout "WEST"))
        (c (jss:get-java-field 'BorderLayout "CENTER")))
    (if north (#"add" parent-container north n))
    (if south (#"add" parent-container south s))
    (if east (#"add" parent-container east e))
    (if west (#"add" parent-container west w))
    (if center (#"add" parent-container center c))))

(defun set-layout (container layout)
  "Set container to the required layout object."
  (#"setLayout" container layout))

(defun gridlayout (rows columns &optional horizontal-gap vertical-gap)
  "Create GridLayout with specified number of rows and columns."
  (if (and horizontal-gap vertical-gap)
      (jss:new 'GridLayout rows columns horizontal-gap vertical-gap)
      (jss:new 'GridLayout rows columns)))

;; flow layout alignments
;; TODO: See if we should use SwingConstants (defined earlier, above)
(defparameter +flow-layout-left+ (jss:get-java-field 'FlowLayout "LEFT"))
(defparameter +flow-layout-right+ (jss:get-java-field 'FlowLayout "RIGHT"))
(defparameter +flow-layout-center+ (jss:get-java-field 'FlowLayout "CENTER"))
(defparameter +flow-layout-leading+ (jss:get-java-field 'FlowLayout "LEADING"))
(defparameter +flow-layout-trailing+ (jss:get-java-field 'FlowLayout "TRAILING"))

(defun flowlayout (flow-layout-alignment &optional horizontal-gap vertical-gap)
  "Create FlowLayout with the specified alignment (use the defined constants for alignment.)"
  (if (and horizontal-gap vertical-gap)
      (jss:new 'FlowLayout flow-layout-alignment horizontal-gap vertical-gap)
      (jss:new 'FlowLayout flow-layout-alignment)))

;; TEMP: Load MigLayout

(defvar *miglayout-loaded* nil)
(defun load-miglayout ()
  (unless *miglayout-loaded*
    (java:add-to-classpath #P"java_libs/miglayout-3.5.5.jar")
    (jss:jar-import "java_libs/miglayout-3.5.5.jar")
    (setf *miglayout-loaded* T)))

(defun miglayout (&optional (layout-constraints "")
                            (column-constraints "")
                            (row-constraints ""))
  "Constructor for a MigLayout."
  (load-miglayout)
  (jss:new "net.miginfocom.swing.MigLayout"
           layout-constraints
           column-constraints
           row-constraints))

