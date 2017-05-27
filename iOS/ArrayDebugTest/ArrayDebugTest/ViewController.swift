//
//  ViewController.swift
//  ArrayDebugTest
//
//  Created by Yusuke on 3/27/16.
//  Copyright © 2016 Yusuke. All rights reserved.
//

import UIKit

class ViewController: UIViewController {

    override func viewDidLoad() {
        super.viewDidLoad()
        // Do any additional setup after loading the view, typically from a nib.
        
        struct Dog : CustomDebugStringConvertible {
            let name: String
            let age: Int
            let type: String
            
            init(name: String, age: Int, type: String) {
                self.name = name
                self.age = age
                self.type = type
            }
            
            var debugDescription: String {
                return "\(name): \(age)"
            }
        }

        let array: [Dog] = [
            Dog(name: "ポチ", age: 3, type: "柴犬"),
            Dog(name: "ジョン", age: 4, type: "ゴールデンレトリバー"),
            Dog(name: "エレナ", age: 2, type: "プードル"),
        ]
        
        print(array) // ここでbreak!
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }


}

