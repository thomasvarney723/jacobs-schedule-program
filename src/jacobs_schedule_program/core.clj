(ns jacobs-schedule-program.core
  (:gen-class))

(defn make-table [seq32-of-string7]
  (let [horiz-line "-----------------"
        columns    "|I|U|M|T|W|R|F|S|"
        single-table (fn [seq16-of-string7]
                       (concat [columns horiz-line]
                               (map (partial apply str)
                                    (map (comp #(str "|" % "|") char) (range 65 81))
                                    (map #(apply str (interleave % (repeat "|"))) seq16-of-string7))
                               [horiz-line]))]
    (->> seq32-of-string7
         (split-at 16)  ;; divide into first and second week
         (mapcat single-table)
         (cons horiz-line)
         (map #(str "\t\t\t" %)))))

(defn sum-team-members [seq-of-team-member-maps]
  (->> seq-of-team-member-maps
       (map :weeks)
       (map (partial apply str))
       (apply (partial map vector))
       (map (partial filter #(= % \X)))
       (map count)
       (partition 7)))

(defn structure-data [string-file]
  (->> (clojure.string/split string-file #"\n")
       rest
       (partition 34)
       (map (juxt first second nnext))
       (map #(zipmap [:name :status :weeks] %))))

(defn -main []
  (print \tab "Which of the following tasks would you like completed?:" "\n"
         "\n\t\t" "1) Import roster of team member and their schedules"
         "\n\t\t" "2) Print the roster of team members"
         "\n\t\t" "3) Print a team member's Schdedule"
         "\n\t\t" "4) Print an overview of the coordinator's schedule"
         "\n\t\t" "5) Print an overview of the load puller's schedule"
         "\n\t\t" "6) Print an overview of the department's schedule"
         "\n\t\t" "7) Quit" "\n\n"
         \tab "What is the number choice of your desired selection?: ")
  (flush)
  (let [in (read-line)]
    (case in
      "1" (do (print (str "\n\t\t" "You chose to generate the roster of team members and their schedules."
                          "\n\n\t\t" \tab "What file should we import team member data from? : "))
              (flush)
              (let [in (read-line)]
                (def data (structure-data (slurp in)))
                (print (str "\n\t\t" "You've successfully created the roster!" "\n\n"))
                (recur)))
      "2" (do (run! print (concat "\n\t\t" "You chose to print the roster of team members."
                                  "\n\t\t" "There are currently " [(count data)] " team member(s) on the roster."
                                  (mapv #(str "\n\t\t" \tab "The name of team member " %1 " is " (:name %2) "." "\n")
                                        (range 1 (inc (count data)))
                                        data)
                                  "\n\t\t" "The roster of team members can be seen above." "\n\n"))
              (recur))
      "3" (do (print (str "\n\t\t" "You chose to print a team member's schedule!" "\n\t\t"
                          "What team member would you like to see the schedule of? (Enter their position in the roster): "))
              (flush)
              (let [in (read-line)]
                (print (str "\n\t\t" "Their schedule looks like this: " "\n"))
                (run! println (->> in Integer. dec (nth data) :weeks make-table))
                (recur)))
      "4" (do (print (str "\n\t\t" "You chose to print the coordinator's schedule!"
                          "\n\t\t" "Their schedule looks like this: " "\n"))
              (run! println (->> data (filter #(= "1" (:status %))) sum-team-members make-table))
              (recur))
      "5" (do (print (str "\n\t\t" "You chose to print the load puller's schedule!"
                          "\n\t\t" "Their schedule looks like this: " "\n"))
              (run! println (->> data (filter #(= "2" (:status %))) sum-team-members make-table))
              (recur))
      "6" (do (print (str "\n\t\t" "You chose to print the entire department schedule!"
                          "\n\t\t" "Their schedule looks like this: " "\n"))
              (run! println (->> data (remove #(= "0" (:status %))) sum-team-members make-table))
              (recur))
      "7" (do (print (str "\t\t" "Please let me know if you need any help in the future."))
              (System/exit 0)))))
