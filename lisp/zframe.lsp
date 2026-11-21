;;; ==================================================================
;;; zframe.lsp  -  1단계: 프레임 찾기 + Z 순서 번호 표시 (출력 없음)
;;;
;;; 명령
;;;   ZF     : ㄱ자 표시(폴리선) 하나를 견본으로 클릭 -> 같은 ㄱ자를 모두 찾아
;;;            프레임마다 빨간 사각형과 번호 표시 (레이어는 조건으로 쓰지 않음)
;;;   ZFCLR  : ZF가 그린 표시(ZF_CHECK 레이어 객체)를 모두 지움
;;;
;;; 사용 함수: 기본 AutoLISP 함수만 사용 (vl- / vla- / vlax- 함수 사용 안 함)
;;; 좌표: 배치탭(용지 공간) LWPOLYLINE의 DXF 10 좌표를 그대로 사용
;;; ==================================================================

;;; ---------- 설정값 (도면 단위, 필요하면 숫자만 바꿔서 다시 로드) ----------
(setq *ZF-TOL-PT*  0.5)          ; 꼭짓점 일치 / 같은 행·열 판단 허용 오차
(setq *ZF-TOL-ROW* 5.0)          ; Z 순서에서 "같은 줄" 판단 Y 허용 오차
(setq *ZF-MARK-LAYER* "ZF_CHECK") ; 확인용 표시를 그릴 레이어 (플롯 안 함으로 생성)

;;; ---------- 작은 도우미 ----------
(defun zf-near (a b tol) (<= (abs (- a b)) tol))
(defun zf-pt2 (p) (list (car p) (cadr p)))
(defun zf-fmt (x y) (strcat "(" (rtos x 2 2) ", " (rtos y 2 2) ")"))

;;; 삽입 정렬 (같은 값은 원래 순서 유지)
(defun zf-ins (e lst lessp)
  (cond ((null lst) (list e))
        ((apply lessp (list e (car lst))) (cons e lst))
        (t (cons (car lst) (zf-ins e (cdr lst) lessp)))))
(defun zf-isort (lst lessp / out)
  (foreach e lst (setq out (zf-ins e out lessp)))
  out)

;;; ---------- 1) ㄱ자 폴리선 모으기 (현재 배치탭, 견본과 같은 것만) ----------
;;; LWPOLYLINE 정보 읽기
(defun zf-pl-pts (ed / r)
  (foreach p ed (if (= (car p) 10) (setq r (cons (zf-pt2 (cdr p)) r))))
  (reverse r))
(defun zf-pl-bulge (ed / b)   ; 곡선 구간(볼록값 42)이 하나라도 있으면 T
  (foreach p ed (if (and (= (car p) 42) (> (abs (cdr p)) 1e-9)) (setq b T)))
  b)
(defun zf-pl-closed (ed) (= 1 (logand 1 (cdr (assoc 70 ed)))))

;;; ㄱ자 판정
;;; 맞으면 (선1 선2 짧은팔길이 긴팔길이), 아니면 탈락 사유 문자열
;;; 선1, 선2는 가운데 점(꼭짓점)에서 양 끝으로 가는 선
(defun zf-mark-shape (ed tol / pts ln1 ln2 hv a b)
  (setq pts (zf-pl-pts ed))
  (cond
    ((/= (length pts) 3) "점 개수 3개 아님")
    ((zf-pl-closed ed) "닫힌 선")
    ((zf-pl-bulge ed) "곡선 구간 있음")
    (t
     (setq ln1 (list (nth 1 pts) (nth 0 pts))
           ln2 (list (nth 1 pts) (nth 2 pts))
           hv  (zf-split (list ln1 ln2) tol))
     (if (and (= (length (car hv)) 1) (= (length (cadr hv)) 1))
       (progn
         (setq a (distance (nth 0 pts) (nth 1 pts))
               b (distance (nth 1 pts) (nth 2 pts)))
         (list ln1 ln2 (min a b) (max a b)))
       "가로+세로 모양 아님"))))

;;; (사유 . 개수) 목록에 1 더하기
(defun zf-count (key al / e)
  (if (setq e (assoc key al))
    (subst (cons key (1+ (cdr e))) e al)
    (cons (cons key 1) al)))

;;; samp : 견본의 zf-mark-shape 결과, color : 견본 색상 번호(nil이면 BYLAYER -> 색 조건 없음)
;;; 결과: (선목록 후보수 탈락목록)   탈락목록: (("사유" . 개수) ...)
(defun zf-get-marks (tab samp color tol / flt ss i ed r lines cnt rej)
  (setq flt (list '(0 . "LWPOLYLINE") (cons 410 tab) '(90 . 3)))
  (if color (setq flt (append flt (list (cons 62 color)))))
  (setq ss (ssget "X" flt)  i 0  cnt 0)
  (if ss
    (while (< i (sslength ss))
      (setq ed  (entget (ssname ss i))
            cnt (1+ cnt)
            r   (zf-mark-shape ed tol))
      (cond
        ((= (type r) 'STR) (setq rej (zf-count r rej)))
        ((not (and (zf-near (nth 2 r) (nth 2 samp) tol)
                   (zf-near (nth 3 r) (nth 3 samp) tol)))
         (setq rej (zf-count "팔 길이 다름" rej)))
        (t (setq lines (cons (car r) (cons (cadr r) lines)))))
      (setq i (1+ i))))
  (list lines cnt rej))

;;; ---------- 2) 가로선 / 세로선 나누기 ----------
;;; 결과: (가로선목록 세로선목록)
(defun zf-split (lines tol / h v dx dy)
  (foreach ln lines
    (setq dx (abs (- (car (cadr ln)) (car (car ln))))
          dy (abs (- (cadr (cadr ln)) (cadr (car ln)))))
    (cond ((and (<= dy tol) (> dx tol)) (setq h (cons ln h)))
          ((and (<= dx tol) (> dy tol)) (setq v (cons ln v)))))
  (list h v))

;;; ---------- 3) 꼭짓점 찾기 + 모서리 유형 분류 ----------
;;; 꼭짓점 기록: ("TL"|"TR"|"BL"|"BR" x y)
;;;   가로선 다른 끝이 왼쪽 -> L, 오른쪽 -> R
;;;   세로선 다른 끝이 위   -> 위쪽 모서리(T), 아래 -> 아래쪽 모서리(B)
;;;   위+왼 = TL(왼쪽위), 위+오른 = TR, 아래+왼 = BL, 아래+오른 = BR
(defun zf-has-corner (c lst tol / found)
  (foreach e lst
    (if (and (= (car e) (car c))
             (zf-near (cadr e) (cadr c) tol)
             (zf-near (caddr e) (caddr c) tol))
      (setq found T)))
  found)

(defun zf-find-corners (hs vs tol / res hp ho vp vo typ c)
  (foreach h hs
    (foreach v vs
      (foreach hi '(0 1)
        (foreach vi '(0 1)
          (setq hp (nth hi h)  ho (nth (- 1 hi) h)
                vp (nth vi v)  vo (nth (- 1 vi) v))
          (if (<= (distance hp vp) tol)
            (progn
              (setq typ (if (> (cadr vo) (cadr vp))
                          (if (< (car ho) (car hp)) "TL" "TR")
                          (if (< (car ho) (car hp)) "BL" "BR")))
              ;; 꼭짓점 X는 세로선, Y는 가로선 값 사용
              (setq c (list typ (car vp) (cadr hp)))
              (if (not (zf-has-corner c res tol))
                (setq res (cons c res)))))))))
  res)

(defun zf-of-type (typ corners / r)
  (foreach c corners (if (= (car c) typ) (setq r (cons c r))))
  r)

;;; ---------- 4) 왼쪽위 기준으로 짝짓기 ----------
;;; 결과: (프레임목록 실패목록)
;;;   프레임: (왼 아래 오른 위)
;;;   실패  : (x y "사유")
(defun zf-build-frames (corners tol / tls trs bls brs frames fails
                                       x0 y0 tr bl br why)
  (setq tls (zf-of-type "TL" corners)
        trs (zf-of-type "TR" corners)
        bls (zf-of-type "BL" corners)
        brs (zf-of-type "BR" corners))
  (foreach tl tls
    (setq x0 (cadr tl)  y0 (caddr tl)  tr nil  bl nil  br nil  why nil)
    ;; 오른쪽위: 같은 줄(Y 일치), 오른쪽, 가장 가까운 것
    (foreach c trs
      (if (and (zf-near (caddr c) y0 tol)
               (> (cadr c) (+ x0 tol))
               (or (null tr) (< (cadr c) (cadr tr))))
        (setq tr c)))
    ;; 왼쪽아래: 같은 열(X 일치), 아래쪽, 가장 가까운 것
    (foreach c bls
      (if (and (zf-near (cadr c) x0 tol)
               (< (caddr c) (- y0 tol))
               (or (null bl) (> (caddr c) (caddr bl))))
        (setq bl c)))
    ;; 오른쪽아래: (오른쪽위 X, 왼쪽아래 Y) 위치
    (if (and tr bl)
      (foreach c brs
        (if (and (zf-near (cadr c) (cadr tr) tol)
                 (zf-near (caddr c) (caddr bl) tol))
          (setq br c))))
    ;; 검증: 짝 사이에 다른 왼쪽위가 끼어 있으면, 자기 짝이 빠져서
    ;;       이웃 프레임 것을 잘못 잡은 것으로 보고 실패 처리
    (if tr
      (foreach c tls
        (if (and (zf-near (caddr c) y0 tol)
                 (> (cadr c) (+ x0 tol))
                 (< (cadr c) (- (cadr tr) tol)))
          (setq why "오른쪽위 표시 누락 의심(이웃 프레임 것과 짝지어짐)"))))
    (if bl
      (foreach c tls
        (if (and (zf-near (cadr c) x0 tol)
                 (< (caddr c) (- y0 tol))
                 (> (caddr c) (+ (caddr bl) tol)))
          (setq why "왼쪽아래 표시 누락 의심(이웃 프레임 것과 짝지어짐)"))))
    (cond
      ((null tr) (setq fails (cons (list x0 y0 "오른쪽위 짝 없음") fails)))
      ((null bl) (setq fails (cons (list x0 y0 "왼쪽아래 짝 없음") fails)))
      ((null br) (setq fails (cons (list x0 y0 "오른쪽아래 짝 없음") fails)))
      (why       (setq fails (cons (list x0 y0 why) fails)))
      (t
        (setq frames
          (cons (list (min x0 (cadr bl))
                      (min (caddr bl) (caddr br))
                      (max (cadr tr) (cadr br))
                      (max y0 (caddr tr)))
                frames)))))
  (list frames fails))

;;; ---------- 5) Z 순서 정렬 ----------
;;; 위쪽(Y 큰) 줄부터, 줄 안에서는 왼쪽(X 작은)부터
(defun zf-zorder (frames tolrow / sorted rows row refy res)
  (setq sorted (zf-isort frames (function (lambda (a b) (> (nth 3 a) (nth 3 b))))))
  (foreach f sorted
    (if (and row (zf-near (nth 3 f) refy tolrow))
      (setq row (cons f row))
      (progn
        (if row (setq rows (cons row rows)))
        (setq row (list f)  refy (nth 3 f)))))
  (if row (setq rows (cons row rows)))
  (foreach r (reverse rows)
    (setq res (append res (zf-isort r (function (lambda (a b) (< (car a) (car b))))))))
  res)

;;; ---------- 6) 확인용 표시 그리기 ----------
(defun zf-ensure-layer (name)
  (if (not (tblsearch "LAYER" name))
    (entmake (list '(0 . "LAYER")
                   '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord")
                   (cons 2 name) '(70 . 0) '(62 . 1) '(6 . "Continuous")
                   '(290 . 0)))))   ; 290=0 : 플롯 안 함

(defun zf-rect (x1 y1 x2 y2 tab lay)
  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(67 . 1)
                 (cons 410 tab) (cons 8 lay) '(62 . 1)
                 '(100 . "AcDbPolyline") '(90 . 4) '(70 . 1)
                 (list 10 x1 y1) (list 10 x2 y1) (list 10 x2 y2) (list 10 x1 y2))))

(defun zf-text (x y h str tab lay)
  (entmake (list '(0 . "TEXT") '(67 . 1) (cons 410 tab) (cons 8 lay) '(62 . 1)
                 (list 10 x y 0.0) (list 11 x y 0.0)
                 (cons 40 h) (cons 1 str) '(72 . 1) '(73 . 2))))

;;; ---------- 명령: ZF ----------
(defun c:ZF (/ *error* t0 tab pick ed r samp color got lines hv corners built
               frames fails ordered n h f)
  (defun *error* (msg)
    (princ (strcat "\n[ZF] 오류: " msg))
    (princ))
  (cond
    ((= (getvar "TILEMODE") 1)
     (princ "\n[ZF] 지금은 모형 탭입니다. 배치탭으로 전환한 뒤 실행하세요."))
    ((/= (getvar "CVPORT") 1)
     (princ "\n[ZF] 뷰포트 안(모형 공간)이 활성 상태입니다. 뷰포트 바깥을 더블클릭하거나 PSPACE 명령으로 전환한 뒤 실행하세요."))
    (t
     (setq tab (getvar "CTAB"))
     ;; 견본 ㄱ자 선택 (ㄱ자 폴리선이 아니면 다시 묻기, 빈 곳 클릭/Enter면 취소)
     (while (and (not samp)
                 (setq pick (entsel "\nㄱ자 표시 하나를 클릭하세요 (빈 곳/Enter = 취소): ")))
       (setq ed (entget (car pick)))
       (cond
         ((/= (cdr (assoc 0 ed)) "LWPOLYLINE")
          (princ (strcat "\n[ZF] 선택한 객체 종류: " (cdr (assoc 0 ed))
                         " (LWPOLYLINE 아님) - 다시 선택하세요.")))
         ((= (type (setq r (zf-mark-shape ed *ZF-TOL-PT*))) 'STR)
          (princ (strcat "\n[ZF] ㄱ자 모양이 아닙니다(" r ") - 다시 선택하세요.")))
         (t (setq samp  r
                  color (cdr (assoc 62 ed))))))
     (if (not samp)
       (princ "\n[ZF] 선택 없음 - 취소했습니다.")
       (progn
         (setq t0 (getvar "DATE"))
         (setq got     (zf-get-marks tab samp color *ZF-TOL-PT*)
               lines   (car got)
               hv      (zf-split lines *ZF-TOL-PT*)
               corners (zf-find-corners (car hv) (cadr hv) *ZF-TOL-PT*)
               built   (zf-build-frames corners *ZF-TOL-PT*)
               frames  (car built)
               fails   (cadr built)
               ordered (zf-zorder frames *ZF-TOL-ROW*))
         ;; 표시 그리기
         (zf-ensure-layer *ZF-MARK-LAYER*)
         (setq n 0)
         (foreach f ordered
           (setq n (1+ n)
                 h (- (nth 3 f) (nth 1 f)))
           (zf-rect (nth 0 f) (nth 1 f) (nth 2 f) (nth 3 f) tab *ZF-MARK-LAYER*)
           (zf-text (/ (+ (nth 0 f) (nth 2 f)) 2.0) (/ (+ (nth 1 f) (nth 3 f)) 2.0)
                    (* h 0.3) (itoa n) tab *ZF-MARK-LAYER*))
         (setq h (if ordered (* 0.1 (- (nth 3 (car ordered)) (nth 1 (car ordered)))) 10.0))
         (foreach e fails
           (zf-text (car e) (cadr e) h "?" tab *ZF-MARK-LAYER*))
         ;; 결과 보고
         (princ (strcat "\n[ZF] 배치탭: " tab
                        "   견본 팔 길이: " (rtos (nth 2 samp) 2 2) " / " (rtos (nth 3 samp) 2 2)
                        "   견본 색상: " (if color (itoa color) "BYLAYER(색 조건 없음)")))
         (princ (strcat "\n[ZF] 후보 폴리선(점 3개" (if color ", 같은 색" "") ") "
                        (itoa (cadr got)) "개 -> ㄱ자 통과 " (itoa (/ (length lines) 2)) "개"))
         (foreach e (caddr got)
           (princ (strcat "\n     탈락: " (car e) " " (itoa (cdr e)) "개")))
         (princ (strcat "\n[ZF] 꼭짓점: 왼쪽위 " (itoa (length (zf-of-type "TL" corners)))
                        ", 오른쪽위 " (itoa (length (zf-of-type "TR" corners)))
                        ", 왼쪽아래 " (itoa (length (zf-of-type "BL" corners)))
                        ", 오른쪽아래 " (itoa (length (zf-of-type "BR" corners)))))
         (princ (strcat "\n[ZF] 프레임 " (itoa (length ordered)) "개, 실패 "
                        (itoa (length fails)) "개"))
         (setq n 0)
         (foreach f ordered
           (setq n (1+ n))
           (princ (strcat "\n  " (itoa n) ": 왼쪽아래 " (zf-fmt (nth 0 f) (nth 1 f))
                          "  오른쪽위 " (zf-fmt (nth 2 f) (nth 3 f))
                          "  크기 " (rtos (- (nth 2 f) (nth 0 f)) 2 2)
                          " x " (rtos (- (nth 3 f) (nth 1 f)) 2 2))))
         (foreach e fails
           (princ (strcat "\n  실패 ? 왼쪽위 " (zf-fmt (car e) (cadr e)) " : " (caddr e))))
         (princ (strcat "\n[ZF] 처리 시간 약 "
                        (rtos (* (- (getvar "DATE") t0) 86400.0) 2 2) "초"))
         (princ "\n[ZF] 표시 지우기: ZFCLR")))))
  (princ))

;;; ---------- 명령: ZFCLR ----------
(defun c:ZFCLR (/ ss i)
  (setq ss (ssget "X" (list (cons 8 *ZF-MARK-LAYER*))))
  (setq i 0)
  (if ss
    (progn
      (while (< i (sslength ss))
        (entdel (ssname ss i))
        (setq i (1+ i)))
      (princ (strcat "\n[ZFCLR] " (itoa i) "개 지움")))
    (princ "\n[ZFCLR] 지울 표시 없음"))
  (princ))

(princ "\n[zframe.lsp] 로드됨. 명령: ZF (프레임 찾기/번호 표시), ZFCLR (표시 지우기)")
(princ)
