;;; ==================================================================
;;; zframe.lsp  -  배치탭의 도면(프레임)들을 장마다 A3 PDF로 출력하고 한 파일로 합치기
;;;
;;; 명령
;;;   PDFMERGE : ㄱ자 표시(폴리선) 하나를 견본으로 클릭 -> 같은 ㄱ자를 모두 찾아
;;;              프레임마다 번호 미리보기 -> 번호 선택 -> 저장 위치 -> 출력 + 합치기
;;;              (합치기는 같은 폴더의 zfmerge.exe 가 함)
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

;;; ---------- 공통: 실행 위치 확인 ----------
;;; 문제 없으면 nil, 있으면 안내 문자열
(defun zf-space-problem ()
  (cond
    ((= (getvar "TILEMODE") 1)
     "지금은 모형 탭입니다. 배치탭으로 전환한 뒤 실행하세요.")
    ((/= (getvar "CVPORT") 1)
     "뷰포트 안(모형 공간)이 활성 상태입니다. 뷰포트 바깥을 더블클릭하거나 PSPACE 명령으로 전환한 뒤 실행하세요.")))

;;; ---------- 공통: 견본 ㄱ자 선택 ----------
;;; ㄱ자 폴리선이 아니면 다시 묻기, 빈 곳 클릭/Enter면 취소
;;; 결과: (견본 색상) 또는 nil
(defun zf-pick-sample (tag / pick ed r samp color)
  (while (and (not samp)
              (setq pick (entsel "\nㄱ자 표시 하나를 클릭하세요 (빈 곳/Enter = 취소): ")))
    (setq ed (entget (car pick)))
    (cond
      ((/= (cdr (assoc 0 ed)) "LWPOLYLINE")
       (princ (strcat "\n" tag " 선택한 객체 종류: " (cdr (assoc 0 ed))
                      " (LWPOLYLINE 아님) - 다시 선택하세요.")))
      ((= (type (setq r (zf-mark-shape ed *ZF-TOL-PT*))) 'STR)
       (princ (strcat "\n" tag " ㄱ자 모양이 아닙니다(" r ") - 다시 선택하세요.")))
      (t (setq samp  r
               color (cdr (assoc 62 ed))))))
  (if samp (list samp color)))

;;; ---------- 공통: 프레임 찾기 + Z 정렬 ----------
;;; 결과: (got corners ordered fails)
;;;   got : zf-get-marks 결과 (선목록 후보수 탈락목록)
(defun zf-detect (tab samp color / got hv corners built)
  (setq got     (zf-get-marks tab samp color *ZF-TOL-PT*)
        hv      (zf-split (car got) *ZF-TOL-PT*)
        corners (zf-find-corners (car hv) (cadr hv) *ZF-TOL-PT*)
        built   (zf-build-frames corners *ZF-TOL-PT*))
  (list got corners (zf-zorder (car built) *ZF-TOL-ROW*) (cadr built)))

;;; ==================================================================
;;; PDF 출력
;;; ==================================================================

;;; ---------- 출력 설정값 ----------
(setq *ZF-PLOTTER* "DWG To PDF.pc3")
(setq *ZF-PAPER*   "ISO 전체 페이지 A3(420.00 x 297.00 mm)")

;;; 파일 이름에서 마지막 "." 뒤(확장자) 떼기
(defun zf-strip-ext (name / i dot)
  (setq i 1)
  (while (<= i (strlen name))
    (if (= (substr name i 1) ".") (setq dot i))
    (setq i (1+ i)))
  (if dot (substr name 1 (1- dot)) name))

;;; 바꿀 시스템 변수 저장 / 복원
(defun zf-sv-save (names / r)
  (foreach nm names (setq r (cons (cons nm (getvar nm)) r)))
  r)
(defun zf-sv-restore (saved)
  (foreach p saved (if (cdr p) (setvar (car p) (cdr p)))))

;;; 프레임 하나를 윈도우 범위로 PDF 출력. 질문 순서는 LT 2026 한글판 -PLOT 손 시험 기록 기준
;;; 결과: T = 명령이 끝까지 진행됨, nil = 질문 순서가 맞지 않아 중간에 취소함
(defun zf-plot-frame (f path)
  (command "-PLOT"
           "Y"                                   ; 상세한 플롯 구성
           ""                                    ; 배치 이름 (현재 배치)
           *ZF-PLOTTER*                          ; 출력 장치
           *ZF-PAPER*                            ; 용지 크기
           "M"                                   ; 용지 단위: 밀리미터
           "L"                                   ; 용지 방향: 가로
           "N"                                   ; 위아래 뒤집기: 아니오
           "W"                                   ; 플롯 영역: 윈도우
           (list (nth 0 f) (nth 1 f))            ;   왼쪽 아래
           (list (nth 2 f) (nth 3 f))            ;   오른쪽 위
           "F"                                   ; 축척: 맞춤
           "C"                                   ; 간격띄우기: 중심
           ""                                    ; 플롯 스타일로 플롯
           ""                                    ; 플롯 스타일 테이블
           ""                                    ; 선가중치로 플롯
           ""                                    ; 선가중치를 축척에 적용
           ""                                    ; 도면 공간 먼저 플롯
           ""                                    ; 도면 공간 객체 숨기기
           path                                  ; 파일 이름
           "N"                                   ; 페이지 설정 변경 사항 저장
           "Y")                                  ; 플롯 진행
  (if (> (getvar "CMDACTIVE") 0)
    (progn (command) nil)                        ; 아직 질문이 남음 -> 취소
    T))

(defun zf-now-ms (/ ms)
  (if (setq ms (getvar "MILLISECS")) ms (* (getvar "DATE") 86400000.0)))

;;; ==================================================================
;;; 출력 + 합치기
;;; ==================================================================
(setq *ZF-CTB*    "")    ; 플롯 스타일(ctb) 이름. ""이면 배치에 지정된 값 그대로
(setq *ZF-MERGER* nil)   ; zfmerge.exe 경로 (처음 실행 때 찾거나 물어봄)

;;; 문자열 도우미
(defun zf-nospace (s / i r c)
  (setq i 1 r "")
  (while (<= i (strlen s))
    (setq c (substr s i 1))
    (if (/= c " ") (setq r (strcat r c)))
    (setq i (1+ i)))
  r)
(defun zf-split-str (s sep / i r cur c)
  (setq i 1 cur "")
  (while (<= i (strlen s))
    (setq c (substr s i 1))
    (if (= c sep)
      (setq r (cons cur r) cur "")
      (setq cur (strcat cur c)))
    (setq i (1+ i)))
  (reverse (cons cur r)))
(defun zf-pos (s ch / i p)   ; ch가 처음 나오는 위치, 없으면 nil
  (setq i 1)
  (while (and (not p) (<= i (strlen s)))
    (if (= (substr s i 1) ch) (setq p i))
    (setq i (1+ i)))
  p)
(defun zf-only-chars (s ok / i good)
  (setq i 1 good T)
  (while (<= i (strlen s))
    (if (not (zf-pos ok (substr s i 1))) (setq good nil))
    (setq i (1+ i)))
  good)
(defun zf-dir-of (path / i p c)   ; 마지막 \ 또는 / 까지 (끝의 \ 포함)
  (setq i 1)
  (while (<= i (strlen path))
    (setq c (substr path i 1))
    (if (or (= c "\\") (= c "/")) (setq p i))
    (setq i (1+ i)))
  (if p (substr path 1 p) ""))
(defun zf-3d (n)
  (cond ((< n 10) (strcat "00" (itoa n)))
        ((< n 100) (strcat "0" (itoa n)))
        (t (itoa n))))

;;; 출력 번호 해석: "" = 전체, "1-5,8" 형식
;;; 결과: 오름차순 번호 목록, 잘못된 입력이면 오류 문자열
(defun zf-parse-sel (s n / r part dash a b err k)
  (setq s (zf-nospace s))
  (if (= s "")
    (progn (setq k n) (while (> k 0) (setq r (cons k r) k (1- k))) r)
    (progn
      (foreach part (zf-split-str s ",")
        (cond
          (err)
          ((or (= part "") (not (zf-only-chars part "0123456789-")))
           (setq err (strcat "잘못된 입력: " part)))
          (t
           (if (setq dash (zf-pos part "-"))
             (setq a (atoi (substr part 1 (1- dash)))
                   b (atoi (substr part (1+ dash))))
             (setq a (atoi part) b a))
           (if (or (< a 1) (> b n) (> a b))
             (setq err (strcat "범위 밖: " part " (1~" (itoa n) ")"))
             (while (<= a b)
               (if (not (member a r)) (setq r (cons a r)))
               (setq a (1+ a)))))))
      (if err err (zf-isort r (function (lambda (x y) (< x y))))))))

;;; 확인용 표시 지우기 / 그리기
(defun zf-clear-marks (/ ss i)
  (setq ss (ssget "X" (list (cons 8 *ZF-MARK-LAYER*))) i 0)
  (if ss (while (< i (sslength ss)) (entdel (ssname ss i)) (setq i (1+ i))))
  i)
(defun zf-draw-marks (ordered fails tab / n h)
  (zf-ensure-layer *ZF-MARK-LAYER*)
  (setq n 0)
  (foreach f ordered
    (setq n (1+ n)
          h (- (nth 3 f) (nth 1 f)))
    (zf-rect (nth 0 f) (nth 1 f) (nth 2 f) (nth 3 f) tab *ZF-MARK-LAYER*)
    (zf-text (/ (+ (nth 0 f) (nth 2 f)) 2.0) (/ (+ (nth 1 f) (nth 3 f)) 2.0)
             (* h 0.3) (itoa n) tab *ZF-MARK-LAYER*))
  (setq h (if ordered (* 0.1 (- (nth 3 (car ordered)) (nth 1 (car ordered)))) 10.0))
  (foreach e fails (zf-text (car e) (cadr e) h "?" tab *ZF-MARK-LAYER*)))

;;; zfmerge.exe 찾기: 이미 찾은 경로 -> AutoCAD 검색 경로 -> 직접 선택
;;; 신뢰할 수 있는 위치(TRUSTEDPATHS) 폴더들에서 zfmerge.exe 찾기
;;; TRUSTEDPATHS 예: "C:\a\lisp;C:\b\..."  (끝의 \... 는 하위 폴더 포함 표시)
(defun zf-find-in-trusted (/ tp r d)
  (if (setq tp (getvar "TRUSTEDPATHS"))
    (foreach d (zf-split-str tp ";")
      (if (and (not r) (/= d ""))
        (progn
          (if (and (>= (strlen d) 4) (= (substr d (- (strlen d) 3)) "\\..."))
            (setq d (substr d 1 (- (strlen d) 4))))
          (if (and (> (strlen d) 0) (/= (substr d (strlen d)) "\\"))
            (setq d (strcat d "\\")))
          (setq r (findfile (strcat d "zfmerge.exe")))))))
  r)

;;; zfmerge.exe 찾기: 이미 찾은 경로 -> 신뢰 위치 -> AutoCAD 검색 경로 -> 직접 선택
(defun zf-find-merger ()
  (cond
    ((and *ZF-MERGER* (findfile *ZF-MERGER*)) *ZF-MERGER*)
    ((setq *ZF-MERGER* (zf-find-in-trusted)) *ZF-MERGER*)
    ((setq *ZF-MERGER* (findfile "zfmerge.exe")) *ZF-MERGER*)
    (t (setq *ZF-MERGER* (getfiled "zfmerge.exe 위치를 선택하세요" "" "exe" 0)))))

;;; 임시 PDF 경로 목록: <폴더>zf_<시각>_001.pdf ...
(defun zf-tmp-paths (dir cnt / id i r p)
  (setq id (substr (rtos (getvar "CDATE") 2 6) 10 6)
        i  1)
  (while (<= i cnt)
    (setq p (strcat dir "zf_" id "_" (zf-3d i) ".pdf"))
    (if (findfile p) (setq p (strcat dir "zf_" id "b_" (zf-3d i) ".pdf")))
    (setq r (cons p r) i (1+ i)))
  (reverse r))

;;; 찾기 결과 보고 (문제 확인용): 견본, 후보/탈락, 꼭짓점, 프레임 좌표, 실패
(defun zf-report (tab samp color det / got corners ordered fails n)
  (setq got (nth 0 det) corners (nth 1 det) ordered (nth 2 det) fails (nth 3 det))
  (princ (strcat "\n[PDFMERGE] 배치탭: " tab
                 "   견본 팔 길이: " (rtos (nth 2 samp) 2 2) " / " (rtos (nth 3 samp) 2 2)
                 "   견본 색상: " (if color (itoa color) "BYLAYER(색 조건 없음)")))
  (princ (strcat "\n[PDFMERGE] 후보 폴리선(점 3개" (if color ", 같은 색" "") ") "
                 (itoa (cadr got)) "개 -> ㄱ자 통과 " (itoa (/ (length (car got)) 2)) "개"))
  (foreach e (caddr got)
    (princ (strcat "\n     탈락: " (car e) " " (itoa (cdr e)) "개")))
  (princ (strcat "\n[PDFMERGE] 꼭짓점: 왼쪽위 " (itoa (length (zf-of-type "TL" corners)))
                 ", 오른쪽위 " (itoa (length (zf-of-type "TR" corners)))
                 ", 왼쪽아래 " (itoa (length (zf-of-type "BL" corners)))
                 ", 오른쪽아래 " (itoa (length (zf-of-type "BR" corners)))))
  (setq n 0)
  (foreach f ordered
    (setq n (1+ n))
    (princ (strcat "\n  " (itoa n) ": 왼쪽아래 " (zf-fmt (nth 0 f) (nth 1 f))
                   "  오른쪽위 " (zf-fmt (nth 2 f) (nth 3 f))
                   "  크기 " (rtos (- (nth 2 f) (nth 0 f)) 2 2)
                   " x " (rtos (- (nth 3 f) (nth 1 f)) 2 2))))
  (foreach e fails
    (princ (strcat "\n  실패 ? 왼쪽위 " (zf-fmt (car e) (cadr e)) " : " (caddr e)
                   "  -> 이 프레임은 출력되지 않습니다.")))
  (princ (strcat "\n[PDFMERGE] 프레임 " (itoa (length ordered)) "개, 실패 " (itoa (length fails)) "개")))

;;; ---------- 명령: PDFMERGE (출력 + 합치기) ----------
(defun c:PDFMERGE (/ *error* saved tab prob ps det ordered fails sel ans out ow
                merger tmps i no f p ok t0 t1 done args)
  (defun *error* (msg)
    (if saved (zf-sv-restore saved))
    (zf-clear-marks)
    (princ (strcat "\n[PDFMERGE] 중단: " msg))
    (princ))
  (cond
    ((setq prob (zf-space-problem))
     (princ (strcat "\n[PDFMERGE] " prob)))
    ((not (setq ps (zf-pick-sample "[PDFMERGE]")))
     (princ "\n[PDFMERGE] 선택 없음 - 취소했습니다."))
    (t
     (setq tab     (getvar "CTAB")
           det     (zf-detect tab (car ps) (cadr ps))
           ordered (nth 2 det)
           fails   (nth 3 det))
     ;; 미리보기: 번호 표시
     (zf-clear-marks)
     (zf-draw-marks ordered fails tab)
     (zf-report tab (car ps) (cadr ps) det)
     (cond
       ((not ordered) (princ "\n[PDFMERGE] 출력할 프레임이 없습니다."))
       (t
        ;; 출력할 번호
        (setq sel "")
        (while (= (type sel) 'STR)
          (setq ans (getstring T (strcat "\n출력할 번호 (Enter = 전체 1~" (itoa (length ordered))
                                          ", 예: 1-5,8): "))
                sel (zf-parse-sel ans (length ordered)))
          (if (= (type sel) 'STR) (princ (strcat "\n[PDFMERGE] " sel " - 다시 입력하세요."))))
        ;; 결과 파일
        (setq out (getfiled "합친 PDF 저장"
                            (strcat (getvar "DWGPREFIX") (zf-strip-ext (getvar "DWGNAME")) "_" tab ".pdf")
                            "pdf" 1))
        (cond
          ((not out) (princ "\n[PDFMERGE] 저장 위치 선택 안 함 - 취소했습니다."))
          ((and (findfile out)
                (progn (initget "Y N")
                       (/= "Y" (getkword "\n같은 이름 파일이 있습니다. 덮어쓸까요? [Y/N] <N>: "))))
           (princ "\n[PDFMERGE] 덮어쓰지 않음 - 취소했습니다."))
          ((not (setq merger (zf-find-merger)))
           (princ "\n[PDFMERGE] zfmerge.exe를 찾지 못해 취소했습니다."))
          (t
           (setq ow (findfile out))
           (initget "Y N")
           (if (= "N" (getkword (strcat "\n" (itoa (length sel)) "장을 출력해서 합칩니다. 진행할까요? [Y/N] <Y>: ")))
             (princ "\n[PDFMERGE] 취소했습니다.")
             (progn
               (setq tmps  (zf-tmp-paths (zf-dir-of out) (length sel))
                     saved (zf-sv-save '("FILEDIA" "OSMODE" "BACKGROUNDPLOT"))
                     t0    (zf-now-ms)
                     i     0
                     ok    T)
               (setvar "FILEDIA" 0)
               (setvar "OSMODE" 0)
               (if (getvar "BACKGROUNDPLOT") (setvar "BACKGROUNDPLOT" 0))
               (while (and ok (< i (length sel)))
                 (setq no (nth i sel)
                       f  (nth (1- no) ordered)
                       p  (nth i tmps)
                       t1 (zf-now-ms))
                 (cond
                   ((not (zf-plot-frame f p))
                    (setq ok nil)
                    (princ (strcat "\n[PDFMERGE] " (itoa no) "번 실패: -PLOT 질문 순서가 예상과 다릅니다.")))
                   ((not (findfile p))
                    (setq ok nil)
                    (princ (strcat "\n[PDFMERGE] " (itoa no) "번 실패: PDF가 만들어지지 않았습니다.")))
                   (t
                    (setq done (cons p done))
                    (princ (strcat "\n[PDFMERGE] " (itoa (1+ i)) "/" (itoa (length sel)) " : "
                                   (itoa no) "번 완료 (" (rtos (/ (- (zf-now-ms) t1) 1000.0) 2 1) "초)"))))
                 (setq i (1+ i)))
               (zf-sv-restore saved)
               (setq saved nil)
               (if (not ok)
                 (progn
                   (princ "\n[PDFMERGE] 출력 중 실패하여 합치지 않았습니다. 만들어진 임시 PDF:")
                   (foreach p (reverse done) (princ (strcat "\n  " p)))
                   (princ "\n[PDFMERGE] F2 창 기록을 보내 주세요."))
                 (progn
                   (setq args (strcat (if ow "--overwrite " "") "--delete \"" out "\""))
                   (foreach p tmps (setq args (strcat args " \"" p "\"")))
                   (princ (strcat "\n[PDFMERGE] 출력 완료 (" (rtos (/ (- (zf-now-ms) t0) 1000.0) 2 1)
                                  "초). 합치기 시작 -> 결과는 잠시 뒤 메시지 창으로 표시됩니다."))
                   (princ (strcat "\n[PDFMERGE] 합칠 파일: " out))
                   (startapp merger args)))))))))
     (zf-clear-marks)))
  (princ))

(princ "\n[zframe.lsp] 로드됨. 명령: PDFMERGE (도면 PDF 출력 + 합치기)")
(princ)
