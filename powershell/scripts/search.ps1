if ($args.length -le 0) {
    "need more arguments..."
    "0:location"
    "1:[pattern]"
} else {
    $location = $args[0]
    if (test-path $location) {
	if ($args.length -le 1) {
	    $pattern = "*"
	} else {
	    $pattern = $args[1]
	}
	    ls -r $location -filter $pattern | sort name | ft directory,name
    } else {
        $location + ": no such location..."
    }
}