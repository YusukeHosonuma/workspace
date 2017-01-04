//
//  Foo.h
//  OCMockSample
//
//  Created by Yusuke on 6/23/15.
//  Copyright © 2015 Yusuke. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface Foo : NSObject

// public
- (NSString *)bar;
- (void)foo;
- (void)fooDelay;

// private
- (void)hoge;

@end
