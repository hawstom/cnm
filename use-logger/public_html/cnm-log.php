<?php
// Getting data.
$ip_address = $_SERVER['REMOTE_ADDR'];

$sql = "INSERT INTO `log_event`
  (
  `le_ip_address`,
  `le_computer_name`,
  `le_bios_date`,
  `le_cnm_version`
  )
 VALUES
  (
  :ip_address,
  :computer_name,
  :bios_date,
  :cnm_version
)";

$data= [
  'ip_address' => $ip_address,
  'computer_name' => $_POST['computer_name'],
  'bios_date' => $_POST['bios_date'],
  'cnm_version' => $_POST['cnm_version']
];

// Connecting, selecting database
$pdo = new PDO('mysql:host=localhost;dbname=jconstru_cnm_use', 'jconstru_cnm_use_logger', 'tdN8$o@34y');

// Performing SQL query
$pdo->beginTransaction();
$s = $pdo->prepare($sql);
$s->execute($data);

$event_log_id = $pdo->lastInsertId();

$command_log = $_POST['command_log'];
$command_log_length = strlen($command_log);
$data = array();

for ($i = 0; $i < $command_log_length; $i++) {
  $uc_count = ord(substr($command_log, $i, 1)) - 1;
  if ($uc_count > 0) {
    $data[] = array(
      'uc_log_event_id' => $event_log_id,
      'uc_command_id' => $i,
      'uc_count' => ord(substr($command_log, $i, 1)) - 1
    );
  }
}

$sql = "INSERT INTO `use_count`
  (
  `uc_log_event_id`,
  `uc_command_id`,
  `uc_count`
  )
 VALUES
  (
  :uc_log_event_id,
  :uc_command_id,
  :uc_count
)";

// Performing SQL query
$s = $pdo->prepare($sql);
foreach ($data as $row) {
  $s->execute($row);
}
$pdo->commit();

