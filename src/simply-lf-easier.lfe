(defmodule simply-lf-easier
  (export all))

(defun e-var (sym)           (tuple 'var sym))
(defun e-app (exprA exprB)   (tuple 'app exprA exprB))
(defun e-lam (sym type expr) (tuple 'lam sym type expr))
(defun e-pi (sym tyA tyB)    (tuple 'pi sym tyA tyB))
(defun e-kind (kinds)        (tuple 'kind kinds))

(defun kind-star () 'star)
(defun kind-box () 'box)

;; (defun ty-base () 'base)
;; (defun ty-arrow (typ1 typ2) (tuple 'arrow typ1 typ2))
(defun allowed-kinds ()
  '((tuple (kind-star) (kind-star))
    (tuple (kind-star) (kind-box))
    (tuple (kind-box) (kind-star))
    (tuple (kind-box) (kind-box))))

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

(defun type-check-red (env a)
  (weak-head-normal-form (type-check env a)))

(defun type-check
  ((env (tuple 'var s)) (find-var env s))
  ((env (tuple 'app f a))
   (let ((tf (type-check-red env f)))
     (case tf
       ((tuple 'pi x at rt)
        (let ((ta (type-check env a)))
          (if (not (beta-eq ta at))
            (tuple 'error (++ '"Bad function argument type: " ta " =/= " at))
            (subst x a rt))))
       (_ (tuple 'error (++ '"Non-function in application: " f))))))
  ((env (tuple 'lam s t e))
   (let* ((t-type-err (type-check env t))
          (envenv (extend-env s t env))
          (te (type-check envenv e))
          (lt (e-pi s t te))
          (lt-type-err (type-check env lt)))
     (cond
      ((?= (tuple 'error _) t-type-err) t-type-err)
      ((?= (tuple 'error _) lt-type-err) lt-type-err)
      ;; Surely there is a better way ...
      ('true lt))))
  ((env (tuple 'pi x a b))
   (let* ((s (type-check-red env a))
          (envenv (extend-env x a env))
          (t (type-check-red envenv b)))
     (if (: lists member (tuple s t) (allowed-kinds))
       (tuple 'error '"Bad abstraction")
       t)))
  ((_ (tuple 'kind 'star))  (tuple 'kind 'box))
  ((_ (tuple 'kind 'box)) (tuple 'error '"Found a box!")))

(defun free-vars-sub (a b)
  (: sets subtract (free-vars a) (: sets add_element b (: sets new))))

(defun free-vars
  (((tuple 'var s))     (: sets add_element s (: sets new)))
  (((tuple 'app f a))   (: sets union (free-vars f) (free-vars a)))
  (((tuple 'lam i t e)) (: sets union (free-vars t) (free-vars-sub e i)))
  (((tuple 'pi i k t))  (: sets union (free-vars k) (free-vars-sub t i)))
  (((tuple 'kind _))      (: sets new)))

(defun subst (v x b)
  (let ((free-vars-x (free-vars x)))
    (letrec-function
        ((clone-sym
          (lambda (e i)
            (let ((vars (: sets union free-vars-x (free-vars e))))
              (letrec-function
                  ((loop (lambda (ii)
                           (if (: sets is_element ii vars) (loop (++ ii "'")) ii))))))))
         (abstr
          (lambda (con i ty e)
            (if (=:= v i)
              (funcall con i ty e)
              (if (: sets is_element i free-vars-x)
                (let* ((ii (clone-sym e i))
                       (ee (subst-var i ii e)))
                  (funcall con ii ty (subst v x ee)))
                (funcall con i ty (subst v x e)))))))
      (cond
       ((?= (tuple 'var i) b) (if (=:= i v) x b))
       ((?= (tuple 'app f a) b) (e-app (subst v x f) (subst v x a)))
       ((?= (tuple 'pi i t e) b) (abstr #'e-pi/3 i t e))
       ((?= (tuple 'lam i t e) b) (abstr #'e-lam/3 i t e))))))

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
  (let-function
      ((app (lambda (f as) (: lists foldl #'e-app/2 f (: lists map #'normal-form/1 as)))))
    (letrec-function
        ((spine (match-lambda
                  (((tuple 'app f a) as) (spine f (cons a as)))
                  (((tuple 'lam s ty e) []) (e-lam s ty (normal-form e)))
                  (((tuple 'lam s _ e) (cons a as)) (spine (subst s a e) as))
                  (((tuple 'pi s k t) as) (app (e-pi s (normal-form k) (normal-form t)) as))
                  ((f as) (app f as))
                  )))
      (spine ee []))))

(defun alpha-eq
  (((tuple 'var v) (tuple 'var vv)) (=:= v vv))
  (((tuple 'app f a) (tuple 'app ff aa)) (and (alpha-eq f ff) (alpha-eq a aa)))
  (((tuple 'lam s _ e) (tuple 'lam ss _ ee)) (alpha-eq e (subst-var ss s ee)))
  (((tuple 'pi s _ e) (tuple 'pi ss _ ee)) (alpha-eq e (subst-var ss s ee)))
  ((_ _) 'false))

(defun beta-eq (e1 e2)
  (alpha-eq (normal-form e1) (normal-form e2)))