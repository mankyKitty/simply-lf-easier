(defmodule simply-lf-easier
  (export all))

(defun initial-env ()
  (tuple 'env (: maps new)))

(defun extend-env
  ((sym ty (tuple 'env e))
   (tuple 'env (: maps put sym ty e))))

(defun find-var
  (((tuple 'env e) sym)
   (case (: maps get sym e)
     ((tuple 'ok ty)
      ty)
     ('error
      (tuple 'error (++ '"Cannot find variable: " sym))))))

(defun type-check
  ((env (tuple 'var s)) (find-var env s))
  ((env (tuple 'app f a))
   (let ((tf (type-check env f)))
     (case tf
       ((tuple 'arrow at rt)
        (let ((ta (type-check env a)))
          (if (=/= ta at)
            (tuple 'error (++ '"Bad function argument type: " ta " =/= " at))
            rt)))
       (_ (tuple 'error (++ '"Non-function in application: " f))))))
  ((env (tuple 'lam s t e))
   (let ((envenv (extend-env s t env)))
     (ty-arrow t (type-check envenv e)))))

(defun e-var (sym)
  (tuple 'var sym))

(defun e-app (exprA exprB)
  (tuple 'app exprA exprB))

(defun e-lam (sym type expr)
  (tuple 'lam sym type expr))

(defun ty-base () 'base)

(defun ty-arrow (typ1 typ2)
  (tuple 'arrow typ1 typ2))
 
(defun free-vars
  (((tuple 'var s)) (: sets add_element s (: sets new)))
  (((tuple 'app f a)) (: sets union (free-vars f) (free-vars a)))
  (((tuple 'lam i _ e)) (: sets subtract (free-vars e) (: sets add_element i (: sets new)))))

(defun subst (v x b)
  (let ((free-vars-x (free-vars x)))
    (let-function
        ((clone-sym
          (lambda (e i)
            (let ((vars (: sets union free-vars-x (free-vars e))))
              (letrec-function
                  ((loop (lambda (ii)
                           (if (: sets is_element i vars) (loop (++ i "'")) ii)))))))))
      (cond
       ((?= (tuple 'var i) b)
        (if (=:= i v) x b))
       ((?= (tuple 'app f a) b)
        (e-app (subst v x f) (subst v x a)))
       ((?= (tuple 'lam i ty e) b)
        (if (=:= v i)
          (e-lam i ty e)
          (if (: sets is_element i free-vars-x)
            (let* ((ii (clone-sym e i))
                   (ee (subst-var i ii e)))
              (e-lam ii ty (subst v x ee)))
            (e-lam i ty (subst v x e)))))))))

(defun subst-var (s ss e)
  (subst s (e-var ss) e))

(defun weak-head-normal-form (ee)
  (letrec-function 
      ((spine (match-lambda
                (((tuple 'app f a) as) (spine f (cons a as)))
                (((tuple 'lam s _ e) (cons a as)) (spine (subst s a e) as))
                ((f as) (: lists foldl #'e-app/2 f as)))))
    (spine ee [])))

(defun normal-form (ee)
  (letrec-function
      ((spine (match-lambda
                (((tuple 'app f a) as) (spine f (cons a as)))
                (((tuple 'lam s ty e) []) (e-lam s ty (normal-form e)))
                (((tuple 'lam s _ e) (cons a as)) (spine (subst s a e) as))
                ((f as) (: lists foldl #'e-app/2 f (: lists map #'normal-form/1 as))))))
    (spine ee [])))

(defun alpha-eq
  (((tuple 'var v) (tuple 'var vv)) (=:= v vv))
  (((tuple 'app f a) (tuple 'app ff aa)) (and (alpha-eq f ff) (alpha-eq a aa)))
  (((tuple 'lam s _ e) (tuple 'lam ss _ ee)) (alpha-eq e (subst-var ss s ee)))
  ((_ _) 'false))

(defun beta-eq (e1 e2)
  (alpha-eq (normal-form e1) (normal-form e2)))