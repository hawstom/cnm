-- phpMyAdmin SQL Dump
-- version 4.9.7
-- https://www.phpmyadmin.net/
--
-- Host: localhost:3306
-- Generation Time: Feb 18, 2023 at 12:36 PM
-- Server version: 10.3.38-MariaDB
-- PHP Version: 7.4.33

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET AUTOCOMMIT = 0;
START TRANSACTION;
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8mb4 */;

--
-- Database: `jconstru_cnm_use`
--

-- --------------------------------------------------------

--
-- Table structure for table `command`
--

CREATE TABLE `command` (
  `command_id` smallint(5) UNSIGNED NOT NULL DEFAULT 0,
  `command_name` varchar(64) DEFAULT NULL,
  `command_appgroup` tinyint(3) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COLLATE=latin1_swedish_ci;

--
-- Dumping data for table `command`
--

INSERT INTO `command` (`command_id`, `command_name`, `command_appgroup`) VALUES
(0, "haws-adl", -1),
(1, "haws-aar", -1),
(2, "haws-attredef", -1),
(3, "haws-at", -1),
(4, "haws-bb", -1),
(5, "haws-xda", -1),
(6, "haws-xra", -1),
(7, "haws-brk", -1),
(8, "haws-bm", -1),
(9, "haws-clean", -1),
(10, "haws-contelev", -1),
(11, "haws-copyrot", -1),
(12, "haws-copyrotdrag", -1),
(13, "haws-md", -1),
(14, "haws-dimsty", -1),
(15, "haws-d1", -1),
(16, "haws-d2", -1),
(17, "haws-dp", -1),
(18, "haws-du", -1),
(19, "haws-dv", -1),
(20, "haws-ht", -1),
(21, "haws-te", -1),
(22, "haws-xx", -1),
(23, "haws-c2", -1),
(24, "haws-ct", -1),
(25, "haws-dd", -1),
(26, "haws-p0", -1),
(27, "haws-ee", -1),
(28, "haws-bf", -1),
(29, "haws-copy", -1),
(30, "haws-cb", -1),
(31, "haws-mp", -1),
(32, "haws-pj", -1),
(33, "haws-r1", -1),
(34, "haws-r2", -1),
(35, "haws-r4", -1),
(36, "haws-r9", -1),
(37, "haws-s", -1),
(38, "haws-ub", -1),
(39, "haws-um", -1),
(40, "haws-vb", -1),
(41, "haws-facnum", -1),
(42, "haws-funky", -1),
(43, "haws-imp_exp", -1),
(44, "haws-incnum", -1),
(45, "haws-xin", -1),
(46, "haws-xout", -1),
(47, "haws-eg", -1),
(48, "haws-egn", -1),
(54, "haws-gb", -1),
(55, "haws-gc", -1),
(56, "haws-invl", -1),
(57, "haws-invr", -1),
(58, "haws-lotel", -1),
(59, "haws-pad", -1),
(60, "haws-rev", -1),
(61, "haws-secb", -1),
(62, "haws-secl", -1),
(63, "haws-secr", -1),
(64, "haws-sect", -1),
(65, "haws-sll", -1),
(66, "haws-slope", -1),
(67, "haws-slr", -1),
(68, "haws-spotel", -1),
(69, "haws-tc", -1),
(70, "haws-tcelev", -1),
(71, "haws-tcelevl", -1),
(72, "haws-tcelevr", -1),
(73, "haws-l0", -1),
(74, "haws-lk0", -1),
(75, "haws-lka", -1),
(76, "haws-lki", -1),
(77, "haws-ofi", -1),
(78, "haws-ula", -1),
(79, "haws-laprn", -1),
(80, "haws-ldr", -1),
(81, "haws-led", -1),
(82, "haws-lengthen", -1),
(83, "haws-lm", -1),
(84, "haws-loadandrun", -1),
(85, "haws-m40", -1),
(86, "haws-m42", -1),
(87, "haws-mc2033", -1),
(88, "haws-ffa", -1),
(89, "haws-hawsalias", -1),
(90, "haws-pgpedit", -1),
(91, "haws-user", -1),
(92, "haws-oo", -1),
(93, "haws-offsetx", -1),
(94, "haws-qs14", -1),
(95, "haws-qs2000", -1),
(96, "haws-qs2004", -1),
(97, "haws-pjl", -1),
(98, "haws-polarset", -1),
(99, "haws-polaroff", -1),
(100, "haws-0", -1),
(101, "haws-1", -1),
(102, "haws-aa", -1),
(103, "haws-adt", -1),
(104, "haws-cet", -1),
(105, "haws-cmd", -1),
(106, "haws-dia", -1),
(107, "haws-fdt", -1),
(108, "haws-mbt", -1),
(109, "haws-qt", -1),
(110, "haws-il", -1),
(111, "haws-io", -1),
(112, "haws-ir", -1),
(113, "haws-it", -1),
(114, "haws-llt", -1),
(115, "haws-mvl", -1),
(116, "haws-mvu", -1),
(117, "haws-ose", -1),
(118, "haws-osi", -1),
(119, "haws-osm", -1),
(120, "haws-osn", -1),
(121, "haws-pslt", -1),
(122, "haws-proto", -1),
(123, "haws-protox", -1),
(124, "haws-rga", -1),
(125, "haws-uf", -1),
(126, "haws-uf0", -1),
(127, "haws-uf1", -1),
(128, "haws-vsr", -1),
(129, "haws-10", -1),
(130, "haws-12", -1),
(131, "haws-setdim10", -1),
(132, "haws-setdim12", -1),
(133, "haws-setup", -1),
(134, "haws-sheet", 0),
(135, "haws-sel", -1),
(136, "haws-ser", -1),
(137, "haws-ssx", -1),
(138, "haws-swap", -1),
(139, "haws-th", -1),
(140, "haws-2x", -1),
(141, "haws-5x", -1),
(142, "haws-9x", -1),
(143, "haws-twz", -1),
(144, "haws-x2", -1),
(145, "haws-zw", -1),
(146, "haws-z0", -1),
(147, "haws-za", -1),
(148, "haws-ze", -1),
(149, "haws-zi", -1),
(150, "haws-zo", -1),
(151, "haws-zv", -1),
(152, "haws-zz", -1),
(153, "haws-2l", 0),
(154, "haws-add", 1),
(155, "haws-aee", -1),
(156, "haws-acres", -1),
(157, "haws-sf", -1),
(158, "haws-aet", -1),
(159, "haws-sm", -1),
(160, "haws-sy", -1),
(161, "haws-a2t", -1),
(162, "haws-att2txt", -1),
(163, "haws-bdl", 0),
(164, "haws-bdp", 0),
(165, "haws-berm", 0),
(166, "haws-bl0", 0),
(167, "haws-bw", 0),
(168, "haws-ca", 1),
(169, "haws-chattrib", 0),
(170, "haws-chcoord", 0),
(171, "haws-chdim", -1),
(172, "haws-chm", -1),
(173, "haws-chnum", -1),
(174, "haws-chgtext", -1),
(175, "haws-cht", -1),
(176, "haws-cl", -1),
(177, "haws-cmpro", 0),
(178, "haws-cmt", 0),
(179, "hcnm-cnm", 1),
(180, "hcnm-cnmkt", 1),
(181, "hcnm-cnmkti", 1),
(183, "hcnm-linkproj", 1),
(184, "testset", 1),
(185, "testget", 1),
(188, "hcnm-notesedit", 1),
(189, "hcnm-cnmlayer", 1),
(190, "hcnm-setnotesbubblestyle", 1),
(191, "haws-phaseedit", 1),
(192, "hcnm-attnoplot", 1),
(193, "hcnm-attplot", 1),
(194, "haws-setnotephases", 1),
(195, "haws-cnmmenu", 1),
(196, "haws-cnmsetup", 1),
(197, "haws-ntpurge", 1),
(198, "haws-boxl", 1),
(199, "haws-cirl", 1),
(200, "haws-dial", 1),
(201, "haws-elll", 1),
(202, "haws-hexl", 1),
(203, "haws-octl", 1),
(204, "haws-penl", 1),
(205, "haws-recl", 1),
(206, "haws-sstl", 1),
(207, "haws-tril", 1),
(208, "haws-tcg", 1),
(209, "haws-txtl", 1),
(210, "hcnm-cnmoptions", 1),
(211, "haws-contvol", 0),
(212, "haws-contxt", 0),
(213, "haws-cs", 1),
(214, "haws-curve", -1),
(215, "haws-dw", 0),
(216, "hcnm-about", -1),
(217, "haws-about", -1),
(218, "haws-orderlicenses", -1),
(221, "haws-eop", -1),
(222, "haws-geodata", -1),
(223, "haws-goto", 0),
(224, "haws-incatt", -1),
(226, "haws-ffi", -1),
(228, "haws-istan", 0),
(229, "haws-ff", -1),
(230, "haws-lk", -1),
(231, "haws-off", 1),
(232, "haws-ffx", -1),
(233, "haws-offx", 1),
(234, "haws-uff", -1),
(235, "haws-uffx", -1),
(236, "haws-uoff", -1),
(237, "haws-uoffx", -1),
(238, "haws-las", 0),
(239, "haws-lar", 0),
(240, "haws-lcp", -1),
(241, "haws-lcpx", -1),
(242, "haws-loop", -1),
(243, "haws-tilde", -1),
(244, "haws-dot", -1),
(245, "haws-none", 0),
(246, "haws-letter", -1),
(247, "haws-lotnum", -1),
(248, "haws-ltc", -1),
(249, "haws-ltb", -1),
(250, "haws-lth", -1),
(251, "haws-ltp", -1),
(252, "haws-ltpx", -1),
(253, "haws-lwp", -1),
(254, "haws-lwpx", -1),
(255, "haws-lx", -1),
(256, "haws-lxx", -1),
(257, "haws-mf", -1),
(258, "haws-mfillet", 0),
(259, "haws-mof", -1),
(260, "haws-moffset", 0),
(261, "haws-mren", -1),
(262, "haws-mrename", 0),
(263, "haws-mscr", -1),
(264, "haws-mscript", 0),
(265, "haws-mv", -1),
(266, "haws-ne", 0),
(267, "haws-na", 0),
(268, "haws-newscale", 0),
(269, "haws-num", -1),
(270, "haws-pipe", 0),
(271, "haws-plt", -1),
(272, "haws-presuf", -1),
(273, "haws-propipe", 0),
(274, "haws-prosup", 0),
(275, "haws-pc", -1),
(276, "haws-procb", 0),
(277, "haws-pm", -1),
(278, "haws-promh", 0),
(279, "haws-pred", -1),
(280, "haws-proe", 0),
(281, "haws-pldr", 0),
(282, "haws-newpro", -1),
(283, "haws-profc", 0),
(284, "haws-pro", -1),
(285, "haws-tgh2_pro", 0),
(286, "haws-lst", -1),
(287, "haws-ellabel", 0),
(288, "haws-stalabel", 0),
(289, "haws-elv", -1),
(290, "haws-grd", -1),
(291, "haws-grc", -1),
(292, "haws-grb", -1),
(293, "haws-pall", -1),
(294, "haws-l80", -1),
(295, "haws-l100", -1),
(296, "haws-l120", -1),
(297, "haws-l140", -1),
(298, "haws-l175", -1),
(299, "haws-l200", -1),
(300, "haws-l240", -1),
(301, "haws-l290", -1),
(302, "haws-l350", -1),
(303, "haws-l500", -1),
(304, "haws-rescale", 0),
(305, "haws-romans", 0),
(306, "haws-rotatebase", -1),
(307, "haws-round", -1),
(308, "haws-ssxpro", -1),
(309, "haws-stacl", 0),
(310, "haws-dm", 0),
(311, "haws-dm12", 0),
(312, "haws-tap", 0),
(313, "haws-tapinv", 0),
(316, "haws-to", 1),
(317, "haws-tu", 1),
(318, "haws-tw", 0),
(319, "haws-txtsum", -1),
(320, "haws-u0", -1),
(321, "haws-u1", -1),
(322, "haws-u2", -1),
(323, "haws-u3", -1),
(324, "haws-u8", -1),
(325, "haws-us", -1),
(326, "haws-ut", 0),
(327, "haws-wall", 0),
(328, "haws-ws", 0),
(329, "haws-wl", -1),
(330, "haws-xd", -1),
(331, "haws-xro", -1),
(332, "haws-xroffset", 0),
(333, "haws-xu", -1),
(334, "haws-xy", 0),
(335, "hcnm-notesedit-pro", 2),
(336, "hcnm-cnmqt", 1),
(337, "hcnm-edit-bubbles", 1),
(1000, "untracked", 1);

-- --------------------------------------------------------

--
-- Table structure for table `log_event`
--

DROP TABLE IF EXISTS `log_event`;
CREATE TABLE IF NOT EXISTS `log_event` (
  `le_id` int(10) UNSIGNED NOT NULL AUTO_INCREMENT,
  `le_date_logged` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `le_ip_address` char(15) DEFAULT NULL,
  `le_computer_name` varchar(255) DEFAULT NULL,
  `le_loginname` varchar(255) DEFAULT NULL,
  `le_cnm_version` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`le_id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `log_event`
--

DROP TABLE IF EXISTS `use_count`;
CREATE TABLE IF NOT EXISTS `use_count` (
  `uc_id` int(10) UNSIGNED NOT NULL AUTO_INCREMENT,
  `uc_log_event_id` int(10) UNSIGNED NOT NULL,
  `uc_command_id` smallint(5) DEFAULT NULL,
  `uc_count` smallint(5) DEFAULT NULL,
  PRIMARY KEY (`uc_id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Stand-in structure for view `view_commands_by_count`
-- (See below for the actual view)
--
DROP VIEW IF EXISTS `view_commands_by_count`;
CREATE TABLE IF NOT EXISTS `view_commands_by_count` (
);

-- --------------------------------------------------------

--
-- Stand-in structure for view `view_commands_by_logger`
-- (See below for the actual view)
--
DROP VIEW IF EXISTS `view_loggers`;
CREATE TABLE IF NOT EXISTS `view_loggers` (
);

-- --------------------------------------------------------

--
-- Stand-in structure for view `view_loggers`
-- (See below for the actual view)
--
DROP VIEW IF EXISTS `view_loggers`;
CREATE TABLE IF NOT EXISTS `view_loggers` (
);

-- --------------------------------------------------------

--
-- Structure for view `view_commands_by_count`
--
DROP TABLE IF EXISTS `view_commands_by_count`;

CREATE ALGORITHM=UNDEFINED DEFINER=`jconstru`@`localhost` SQL SECURITY DEFINER VIEW `view_commands_by_count`  AS  
select 
  `c`.`command_name` AS `Name`,
	`c`.`command_id` AS `Id`, 
	sum(`uc`.`uc_count`) AS `Count` 
from (
  `command` `c` 
	join 
	`use_count` `uc`
) 
where (
  `c`.`command_id` = `uc`.`uc_command_id`
) 
group by 
  `c`.`command_name` 
order by 
  sum(`uc`.`uc_count`) desc ;

-- --------------------------------------------------------

--
-- Structure for view `view_commands_by_logger`
--
DROP TABLE IF EXISTS `view_commands_by_logger`;

CREATE ALGORITHM=UNDEFINED DEFINER=`jconstru`@`localhost` SQL SECURITY DEFINER VIEW `view_commands_by_logger`  AS  
select 
	`c`.`command_name` AS `Command name`, 
	`c`.`command_id` AS `Id`, 
	`le`.`le_ip_address` AS `IP Address`, 
	`le`.`le_computer_name` AS `Computer name`, 
	`le`.`le_loginname` AS `Loginname`, 
	sum(`uc`.`uc_count`) AS `Count` 
from (
	`command` `c`, 
	`use_count` `uc`, 
	`log_event` `le`
) 
where (
	`c`.`command_id` = `uc`.`uc_command_id` 
	AND 
	`uc`.`uc_log_event_id` = `le`.`le_id`
) 
group by 
	`le`.`le_ip_address`,
	`le`.`le_computer_name`, 
	`le`.`le_loginname`, 
	`c`.`command_name` 
order by sum(
	`uc`.`uc_count`
) 
desc;

-- --------------------------------------------------------

--
-- Structure for view `view_loggers`
--
DROP TABLE IF EXISTS `view_loggers`;

CREATE ALGORITHM=UNDEFINED DEFINER=`jconstru`@`localhost` SQL SECURITY DEFINER VIEW `view_loggers` AS  
select 
	`le`.`le_ip_address` AS `IP Address`, 
	`le`.`le_computer_name` AS `Computer name`, 
	`le`.`le_loginname` AS `Loginname`, 
	COUNT(`le`.`le_id`) AS `Count` 
from (`log_event` `le`) 
group by 
	`le`.`le_ip_address`,
	`le`.`le_computer_name`, 
	`le`.`le_loginname` 
order by 
	`le`.`le_ip_address`,
	`le`.`le_computer_name`, 
	`le`.`le_loginname` ;
COMMIT;
-- DELETE uc, le FROM use_count AS uc INNER JOIN log_event AS le ON le.le_id=uc.uc_log_event_id WHERE le.le_computer_name = 'GERANIUM'
