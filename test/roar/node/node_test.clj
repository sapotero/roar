(ns roar.node.node-test
  (:refer-clojure :exclude [read])
  (:require [clojure.test    :refer :all]
            [roar.node.node  :refer :all]))

(def invalid-slave-msg
  #"Assert failed: \(and \(satisfies\? Node slave\) \(not \(satisfies\? MasterNode slave\)\)\)")

(deftest master-node-test

  (testing "A master node should be created with the correct values"
    (let [expected {:name "daredevil"}]
      (is (.equals expected (-> (master-node "daredevil")
                                (dissoc :rw-strategy)
                                (dissoc :slaves))))))

  (testing "A master node should accept a write and store the key/value,
           it should be retrievable immediately."
    (let [node (master-node "daredevil")]
      (write! node "key" "String data")
      (is (= (read node "key") "String data"))))

  (testing "A master node should accept a write and replicate it to slaves."
    (let [slave (slave-node "foggy")
          master (master-node "daredevil")
          slave2 (slave-node "fisk")]
      (add-slave! master slave)
      (add-slave! master slave2)
      (write! master "key" "String data")
      (is (and (= (read slave "key") "String data")
               (= (read slave2 "key") "String data")))))

  (testing "Adding a slave fails if the slave does not implement the node protocol"
    (let [master (master-node "daredevil")
          slave {}]
      (is (thrown-with-msg?
            AssertionError
            invalid-slave-msg
            (add-slave! master slave)))))

  (testing "Adding a slave fails if the slave implements the master protocol"
    (let [master (master-node "daredevil")
          slave (master-node "fisk")]
      (is (thrown-with-msg?
            AssertionError
            invalid-slave-msg
            (add-slave! master slave))))))

(deftest slave-node-test

  (testing "A slave node should be created with the correct values"
    (let [master (master-node "daredevil")
          slave (slave-node "foggy")
          slave (first @(:slaves (add-slave! master slave)))
          expected {:name "foggy"
                    :master master}]
      (is (.equals expected (-> slave
                                (dissoc :rw-strategy))))))

  (testing "A slave node with a preset master should be created with the correct values"
    (let [master (master-node "daredevil")
          slave (slave-node "foggy" master)
          slave (first @(:slaves (add-slave! master slave)))
          expected {:name "foggy"
                    :master master}]
      (is (.equals expected (-> slave
                                (dissoc :rw-strategy))))))

  (testing "A slave node should accept a write on masters behalf"
    (let [slave (slave-node "foggy")
          master (master-node "daredevil")]
      (add-slave! master slave)
      (write! slave "key" "String data")
      (is (= (read master "key") "String data"))))

  (testing "A new slave node should refresh itself"
    (let [slave (slave-node "foggy")
          master (master-node "daredevil")
          slave2 (slave-node "fisk")]
      (add-slave! master slave)
      (write! slave "key" "String data")
      (write! slave "key2" "String data 2")
      (add-slave! master slave2)
      (is (and (= (read slave2 "key") "String data")
               (= (read slave2 "key2") "String data 2"))))))