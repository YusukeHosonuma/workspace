# Source

```objc
// NSUserDefaultsに保存・更新する
NSUserDefaults *ud = [NSUserDefaults standardUserDefaults];  // 取得
[ud setInteger:100 forKey:@"KEY_I"];  // int型の100をKEY_Iというキーで保存
[ud setFloat:1.23 forKey:@"KEY_F"];  // float型の1.23をKEY_Fというキーで保存
[ud setDouble:1.23 forKey:@"KEY_D"];  // double型の1.23をKEY_Dというキーで保存
[ud setBool:YES forKey:@"KEY_B"];  // BOOL型のYESをKEY_Bというキーで保存
[ud setObject:@"あいう" forKey:@"KEY_S"];  // "あいう"をKEY_Sというキーで保存
[ud synchronize];  // NSUserDefaultsに即時反映させる（即時で無くてもよい場合は不要）
```

# Result

```javascript
var ud = R.NSClassFromString('NSUserDefaults').call('standardUserDefaults');
ud.call('setInteger:forKey:', 100, 'KEY_I');
ud.call('setFloat:forKey:', 1.23, 'KEY_F');
ud.call('setDouble:forKey:', 1.23, 'KEY_D');
ud.call('setBool:forKey:', true, 'KEY_B');
ud.call('setObject:forKey:', 'あいう', 'KEY_S');
ud.call('synchronize');
```