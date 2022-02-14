-- Lists in desc order the commands that are used by people at least 2 times
select t1.Name, count(t1.Name) as `Count` from
(
select `le`.`le_computer_name`, `le`.`le_bios_date`, `c`.`command_name` AS `Name`,`c`.`command_id` AS `Id`, sum(`uc`.`uc_count`) AS `Count` 
from (`command` `c` join `use_count` `uc` join `log_event` `le`) 
where (`c`.`command_id` = `uc`.`uc_command_id` AND `le`.`le_id` = `uc`.`uc_log_event_id`) 
group by `uc`.`uc_command_id`, `le`.`le_computer_name`, `le`.`le_bios_date`
having sum(`uc`.`uc_count`) > 1000
order by sum(`uc`.`uc_count`) desc
) t1
group by Name
order by count(Name) desc

-- Lists who uses this command at least 1000 times
select `le`.`le_computer_name` AS `Computer name`, `le`.`le_bios_date` AS `BIOS_date`, sum(`uc`.`uc_count`) AS `Count` 
from (`command` `c`, `use_count` `uc`, `log_event` `le`) 
where (`c`.`command_id` = `uc`.`uc_command_id` AND `uc`.`uc_log_event_id` = `le`.`le_id` AND `c`.`command_name` = 'haws-aet') 
group by `le`.`le_computer_name`, `le`.`le_bios_date`, `c`.`command_name` 
having sum(`uc`.`uc_count`) > 1000
order by sum(`uc`.`uc_count`) desc;