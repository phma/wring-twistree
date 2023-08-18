// Module keyschedule
// This module is used in both Wring and Twistree.
// It converts a slice of bytes of arbitrary length, which should not exceed 96,
// into an array of 96 u16. Then it reschedules the array for the next s-box.
