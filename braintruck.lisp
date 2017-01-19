(defpackage :braintruck
  (:use :cl)
  (:export #:braintruck))
(in-package :braintruck)

;;; Utilities -----------------------------------------------------------------

(defparameter +instruction-characters+ '(#\> #\< #\+ #\- #\. #\, #\[ #\]))

(let ((label-counter 0))
  (defun make-label ()
    (format nil ".L~A" (incf label-counter))))

;;; Instructions --------------------------------------------------------------

(defclass instruction () ())

(defclass move-instruction (instruction) ())
(defclass move-right-instruction (move-instruction) ())
(defclass move-left-instruction (move-instruction) ())

(defclass modify-instruction (instruction) ())
(defclass increment-instruction (modify-instruction) ())
(defclass decrement-instruction (modify-instruction) ())

(defclass io-instruction (instruction) ())
(defclass input-instruction (io-instruction) ())
(defclass output-instruction (io-instruction) ())

(defclass jump-instruction (instruction)
  ((label :initform (make-label) :reader label)
   (target :initform nil :accessor target)))
(defclass jump-forward-instruction (jump-instruction) ())
(defclass jump-backward-instruction (jump-instruction) ())

(defun make-instruction (character)
  (make-instance (ecase character
                   (#\> 'move-right-instruction)
                   (#\< 'move-left-instruction)
                   (#\+ 'increment-instruction)
                   (#\- 'decrement-instruction)
                   (#\, 'input-instruction)
                   (#\. 'output-instruction)
                   (#\[ 'jump-forward-instruction)
                   (#\] 'jump-backward-instruction))))

(defun connect-jump-instructions (instructions)
  (loop with stack = ()
        for instruction in instructions
        do (typecase instruction
             (list (break))
             (jump-forward-instruction
              (push instruction stack))
             (jump-backward-instruction
              (setf (target instruction) (first stack))
              (setf (target (pop stack)) instruction)))
        finally (unless (null stack)
                  (error "Jumps mismatched"))))

(defun convert-to-instructions (input)
  (loop for character across input
        collect (make-instruction character)))

;;; Instruction emission ------------------------------------------------------

(defvar *asm-stream*)

(defun emit (format &rest args)
  (apply #'format *asm-stream* format args)
  (terpri *asm-stream*))

(defgeneric emit-instruction (instruction))

(defmethod emit-instruction ((instruction move-right-instruction))
  (emit "sub rbx, 8"))
(defmethod emit-instruction ((instruction move-left-instruction))
  (emit "add rbx, 8"))

(defmethod emit-instruction ((instruction increment-instruction))
  (emit "add [rbx], DWORD 1"))

(defmethod emit-instruction ((instruction decrement-instruction))
  (emit "sub [rbx], DWORD 1"))

(defmethod emit-instruction ((instruction input-instruction))
  (emit "mov rax, 0")
  (emit "mov rdi, 0")
  (emit "mov rsi, rbx")
  (emit "mov rdx, 1")
  (emit "syscall"))

(defmethod emit-instruction ((instruction output-instruction))
  (emit "mov rax, 1")
  (emit "mov rdi, 1")
  (emit "mov rsi, rbx")
  (emit "mov rdx, 1")
  (emit "syscall"))

(defmethod emit-instruction ((instruction jump-forward-instruction))
  (emit "~A:" (label instruction))
  (emit "cmp [rbx], DWORD 0")
  (emit "je ~A" (label (target instruction))))

(defmethod emit-instruction ((instruction jump-backward-instruction))
  (emit "~A:" (label instruction))
  (emit "cmp [rbx], DWORD 0")
  (emit "jne ~A" (label (target instruction))))

(defun emit-prologue ()
  (emit "global _start")
  (emit "section .text")
  (emit "_start:")
  (emit "mov rbx, rsp")
  (emit "mov [rbx], BYTE 0"))

(defun emit-epilogue ()
  (emit "mov eax, 60")
  (emit "xor rdi, rdi")
  (emit "syscall"))

;;; New math ------------------------------------------------------------------

(defun filter-raw-input (raw-input)
  (remove-if (lambda (character)
               (not (member character +instruction-characters+)))
             raw-input))

(defun braintruck (raw-input)
  (let* ((input (filter-raw-input raw-input))
         (instructions (convert-to-instructions input)))
    (connect-jump-instructions instructions)
    (emit-prologue)
    (mapc #'emit-instruction instructions)
    (emit-epilogue)))
