<?php

header('Access-Control-Allow-Origin: *');
header('Access-Control-Allow-Headers: Content-Type');
header('Content-Type: application/json');
echo file_get_contents('http://people.cs.kuleuven.be/~bob.reynders/2015-2016/emails.json');

?>
