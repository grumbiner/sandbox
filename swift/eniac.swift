//
//  eniac.swift
//  hello
//
//  Created by Robert Grumbine on 5/25/26.
//

class eniac {
    var p, q : Int
    var ratio : Int
    var s : [[Float]]
    var t : [[Float]]
    var v : [[Float]]
    var r : [[Float]]
    var f : [[Float]]
    var h : [[Float]]
    var z : [[Float]]
    var zeta : [[Float]]
    var eta : [[Float]]

    let g = 9.81
    let radius = 6.371e6
    let omega = 7.292e-5
    let dt = 7.292e-5*86400/24
    let fttom = 3.048
    let pi = 3.1415926535898
    
    init(p: Int = 19, q: Int = 16, width:Int, height:Int) {
        var k : Int
        self.ratio = min(Int(width/p), Int(height/q))
        print("ratio = ",self.ratio)

        self.p = p*self.ratio
        self.q = q*self.ratio
        self.s = [[Float]](repeating: [Float](repeating: 0, count: self.p), count: self.q)
        self.t = [[Float]](repeating: [Float](repeating: 0, count: self.p), count: self.q)
        self.v = [[Float]](repeating: [Float](repeating: 0, count: self.p), count: self.q)
        self.r = [[Float]](repeating: [Float](repeating: 0, count: self.p), count: self.q)
        self.f = [[Float]](repeating: [Float](repeating: 0, count: self.p), count: self.q)
        self.h = [[Float]](repeating: [Float](repeating: 0, count: self.p), count: self.q)
        self.z = [[Float]](repeating: [Float](repeating: 0, count: self.p), count: self.q)
        self.zeta = [[Float]](repeating: [Float](repeating: 0, count: self.p), count: self.q)
        self.eta = [[Float]](repeating: [Float](repeating: 0, count: self.p), count: self.q)

        // Confirm that arrays are initialized and can be indexed 
        k = 0
        for j in 0...self.p-1 {
            for i in 0...self.q-1 {
                k += 1
                print(i,j,k, self.g, self.s[i][j])
                // can't swap between 1 and 2 indices: print(i,j,k, self.g, self.s[k])
            }
        }

    }
    func show_ratio() -> Int {
        return self.ratio
    }
 
    
}

var y = eniac(width: 300, height: 300)
