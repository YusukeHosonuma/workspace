//
//  ViewController.swift
//  UNNotificationSample
//
//  Created by Yusuke on 12/11/16.
//  Copyright © 2016 Yusuke. All rights reserved.
//

import UIKit
import UserNotifications

class ViewController: UIViewController, UNUserNotificationCenterDelegate {

    override func viewDidLoad() {
        super.viewDidLoad()
        // Do any additional setup after loading the view, typically from a nib.
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }

    // MARK: - Private
    
    private func alert(title: String, message: String) {
        let alert = UIAlertController(title: title, message: message, preferredStyle: UIAlertControllerStyle.alert)
        alert.addAction(UIAlertAction(title: "OK", style: UIAlertActionStyle.cancel, handler: nil))
        self.present(alert, animated: true, completion: nil)
    }
    
    // MARK: - UNUserNotificationCenterDelegate
    
    func userNotificationCenter(_ center: UNUserNotificationCenter, willPresent notification: UNNotification, withCompletionHandler completionHandler: @escaping (UNNotificationPresentationOptions) -> Void) {
        
        // アプリがforegroundの時は、delegateを使って自前で検出する必要あり
        // （アプリがbackgroundのときは、このdelegateは呼び出されない）
        alert(title: "willPresent", message: "アプリがforeground状態でPUSH通知を検出")
    }
    
    func userNotificationCenter(_ center: UNUserNotificationCenter, didReceive response: UNNotificationResponse, withCompletionHandler completionHandler: @escaping () -> Void) {
        
        // 通知がタップされた時に呼び出される
        
        let id = response.notification.request.identifier
        alert(title: "didReceive", message: "通知がタップされた。（\(id)）")
    }
    
    // MARK: - Action
    
    @IBAction func tapRequestPushButton() {
        UNUserNotificationCenter.current().requestAuthorization(options: [.badge, .sound, .alert]) { [unowned self] (granted, error) in
            let message = (granted) ? "承認" : "非承認"
            let alert = UIAlertController(title: "ローカル通知", message: message, preferredStyle: UIAlertControllerStyle.alert)
            alert.addAction(UIAlertAction(title: "OK", style: UIAlertActionStyle.cancel, handler: nil))
            self.present(alert, animated: true, completion: nil)
        }
    }
    
    /// 指定したn秒後に発火
    @IBAction func tapLocalPushButton() {
        
        let content = UNMutableNotificationContent()
        content.title = "ローカル通知"
        content.body = "指定したn秒後に発火"
        content.sound = UNNotificationSound.default()
        
        let trigger = UNTimeIntervalNotificationTrigger(timeInterval: 5, repeats: false)
        let request = UNNotificationRequest(identifier: "sample_identifier",
                                            content: content,
                                            trigger: trigger)
        
        UNUserNotificationCenter.current().add(request) { (error) in
            print("error: \(error)")
        }
        
        UNUserNotificationCenter.current().delegate = self
    }
    
    /// 指定した日時で発火
    @IBAction func tapLocalPushButton2() {
        
        let content = UNMutableNotificationContent()
        content.title = "ローカル通知"
        content.body = "指定した日付で発火"
        content.sound = UNNotificationSound.default()
        
        var component = DateComponents()
        component.hour = 22
        component.minute = 11
        
        let trigger = UNCalendarNotificationTrigger(dateMatching: component, repeats: false)

        let request = UNNotificationRequest(identifier: "sample_identifier_component",
                                            content: content,
                                            trigger: trigger)
        
        UNUserNotificationCenter.current().add(request) { (error) in
            print("error: \(error)")
        }
    }
}

