//
//  Foo.m
//  OCMockSample
//
//  Created by Yusuke on 6/23/15.
//  Copyright © 2015 Yusuke. All rights reserved.
//

#import "Foo.h"

@implementation Foo

#pragma mark - Public methods

- (NSString *)bar {
    return @"bar!";
}

- (void)foo {
    [self hoge];
}

- (void)fooDelay {
    [self performSelector:@selector(hoge) withObject:nil afterDelay:0.1];
}

#pragma mark - Private methods

- (void)hoge {
    // do nothing.
}

@end
