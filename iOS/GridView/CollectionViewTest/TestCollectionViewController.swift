//
//  TestCollectionViewController.swift
//  CollectionViewTest
//
//  Created by Yusuke on 11/13/14.
//  Copyright (c) 2014 yusuke.hosonuma. All rights reserved.
//

import UIKit

let reuseIdentifier = "Cell"

class TestCollectionViewController: UICollectionViewController {

    var modeIndex = 0
    
    override func viewDidLoad() {
        super.viewDidLoad()
        self.collectionView?.register(UICollectionViewCell.self, forCellWithReuseIdentifier: reuseIdentifier)
        
        self.change();
     
        let changeButton = UIBarButtonItem(title: "change", style: UIBarButtonItemStyle.plain, target: self, action:#selector(TestCollectionViewController.change))
        self.navigationItem.rightBarButtonItem = changeButton
    }
    
    func change() {

        self.modeIndex = (self.modeIndex + 1) % 2

        switch (modeIndex) {
        case 0:
            let layout = UICollectionViewFlowLayout();
            layout.itemSize = CGSize(width:150, height: 150)
            self.collectionView?.setCollectionViewLayout(layout, animated: true)

        case 1:
            let layout = UICollectionViewFlowLayout();
            layout.itemSize = CGSize(width:320, height: 80)
            self.collectionView?.setCollectionViewLayout(layout, animated: true)
            
        default:
            break
        }
        
        self.collectionView?.reloadData()
        
        self.collectionView?.visibleCells.forEach { cell in
            if let indexPath = self.collectionView?.indexPath(for: cell) {
                self.updateCell(cell, indexPath:indexPath)
            }
        }
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
    }

    override func numberOfSections(in collectionView: UICollectionView) -> Int {
        return 1
    }


    override func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return 10
    }

    override func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: reuseIdentifier, for: indexPath) 
        self.updateCell(cell, indexPath: indexPath)
    
        return cell
    }
    
    func updateCell(_ cell: UICollectionViewCell, indexPath: IndexPath) {
        
        switch (modeIndex) {
        case 0:
            cell.backgroundColor = UIColor.blue
            break
            
        case 1:
            cell.backgroundColor = UIColor.red
            break
            
        default:
            break
        }
    }
}
