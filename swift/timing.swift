//
//  eniac.swift
//  hello
//
//  Created by Robert Grumbine on 5/25/26.
//

import Foundation

class eniac {
    var p, q : Int
    var ratio : Int
    var s : [[Int]]

    init(p: Int = 19, q: Int = 16, width:Int, height:Int) {
        var k : Int
        self.ratio = min(Int(width/p), Int(height/q))
        print("ratio = ",self.ratio)

        self.p = p*self.ratio
        self.q = q*self.ratio
        self.s = [[Int]](repeating: [Int](repeating: 0, count: self.p), count: self.q)

        // try timing i,j or j,i loop order -- no discernable difference
        k = 0
        let start = ProcessInfo.processInfo.systemUptime
        for i in 0...self.q-1 {
            for j in 0...self.p-1 {
                k += 1
                self.s[i][j] = k
            }
        }
        let end = ProcessInfo.processInfo.systemUptime
        let delta = end - start
        print("the loop took \(delta) seconds")
        print("\(self.p*self.q) elements in array")
    }
    
}

var y = eniac(width: 3000, height: 3000)
