
(import '(java.awt Color Graphics RenderingHints)
        '(java.awt.image BufferedImage)
        '(java.awt.event MouseListener)
        '(javax.swing JFrame JPanel))

(def c-min             [-20 -25])
(def c-max             [ 25  30])
(def dim-axis          (map #(Math/abs (- %1 %2)) c-min c-max))
(def dim-screen        [800 700])
(def iterations        100)
(def running           (atom true))
(def n                 (atom 0))
(def u                 (atom 0.902))

(def axis-seqs [(iterate #(+ (/ (first dim-axis) (first dim-screen)) %) (first c-min))
                (iterate #(+ (/ (last dim-axis)  (last dim-screen))  %) (last  c-min))])

(defn ikeda [x y u]
     (iterate (fn [[x_n y_n]]
                   (let [t_n (- 0.4 (/ 6 (+ 1 (* x_n x_n) (* y_n y_n))))]
                     [(inc (* u (- (* x_n (Math/cos t_n))
                                   (* y_n (Math/sin t_n)))))
                           (* u (+ (* x_n (Math/sin t_n))
                                   (* y_n (Math/cos t_n ))))]))
              [x y]))

(def spawns    (for [i (range 200)]
                 (let [x (nth (first axis-seqs) (rand-int (first dim-screen)))
                       y (nth (last  axis-seqs) (rand-int (last dim-screen)))]
                   (atom (ikeda x y @u)))))

(defn >screencoordinate [coordinate]
  (map #(* (/ (- %1 %2) (- %3 %2)) %4) coordinate c-min c-max dim-screen))

(defn draw-ikeda-map [#^Graphics canvas]
  (when (zero? @n)
    (doto canvas
      (.setColor Color/WHITE)
      (.fillRect 0 0 (first dim-screen) (last dim-screen))))
  (let [point-color     (int (+ 155 (* (/ 100  iterations) (- @n iterations))))]
    (.setColor canvas (Color. point-color point-color point-color))
    (doseq [spawnpoint spawns]
      (let [[x1 y1]  (>screencoordinate (first  @spawnpoint))
            [x2 y2]  (>screencoordinate (second @spawnpoint))]
        (.drawLine canvas x1 y1 x2 y2)))
    (doto canvas
      (.setColor Color/WHITE)
      (.fillRect 0 3 100 30)
      (.setColor Color/BLACK)
      (.drawRect -5 2 101 31))
    (.drawString canvas (format "u: %5f2" @u) 3 15)
    (.drawString canvas (format "iterations: %d"   @n) 3 28)))

(defn render  [g]
  (let [img (BufferedImage. (first dim-screen) (last dim-screen) BufferedImage/TYPE_INT_ARGB)
        bg  (.getGraphics img)]
    (.setRenderingHint bg RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
    (draw-ikeda-map bg)
    (.drawImage g img 0 0 nil)
    (.dispose bg)))

(defn animate [surface]
  (doseq [point spawns] (swap! point next))
  (swap! n inc)
  (when (>= @n iterations)
    (swap! u #(+ % (- 0.05 (float (/ (rand-int 100) 1000)))))
    (reset! n 0)
    (doseq [point spawns]
      (let [x (nth (first axis-seqs) (rand-int (first dim-screen)))
            y (nth (last  axis-seqs) (rand-int (last dim-screen)))]
        (reset! point (ikeda x y @u)))))
  (.repaint surface)
  (Thread/sleep 20)
  (when @running (recur surface)))

(let [frame    (JFrame. "Ikeda map")
      panel    (doto (proxy [JPanel] [] (paint [g] (render g))))]
  (doto frame (.add panel) .pack (.setSize (first dim-screen) (last dim-screen)) .show
              (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE))
  (-> #(animate panel) Thread. .start))
