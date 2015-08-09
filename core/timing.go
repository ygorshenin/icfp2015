package core

import "time"

var StartTime time.Time
var TimeLimit time.Duration


func TimeOut() error {
	if time.Since(StartTime)+time.Millisecond*500 >= TimeLimit {
		return GameOver
	}
	return nil
}