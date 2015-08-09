package core

import "time"

var StartTime time.Time
var TimeLimit time.Duration


func AlmostNoTimeLeft() bool {
	return time.Since(StartTime)+time.Millisecond*500 >= TimeLimit
}

func Timeout(st time.Time, timeout time.Duration) bool {
	return time.Since(st) >= timeout
}