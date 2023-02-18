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
  `command_appgroup` tinyint(3) UNSIGNED DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COLLATE=latin1_swedish_ci;

--
-- Dumping data for table `command`
--

INSERT INTO `command` (`command_id`, `command_name`, `command_appgroup`) VALUES
(0, 'haws-adl', 0),
(1, 'haws-aar', 0),
(2, 'haws-attredef', 0),
(3, 'haws-at', 0),
(4, 'haws-bb', 0),
(5, 'haws-xda', 0),
(6, 'haws-xra', 0),
(7, 'haws-brk', 0),
(8, 'haws-bm', 0),
(9, 'haws-clean', 0),
(10, 'haws-contelev', 0),
(11, 'haws-copyrot', 0),
(12, 'haws-copyrotdrag', 0),
(13, 'haws-md', 0),
(14, 'haws-dimsty', 0),
(15, 'haws-d1', 0),
(16, 'haws-d2', 0),
(17, 'haws-dp', 0),
(18, 'haws-du', 0),
(19, 'haws-dv', 0),
(20, 'haws-ht', 0),
(21, 'haws-te', 0),
(22, 'haws-xx', 0),
(23, 'haws-c2', 0),
(24, 'haws-ct', 0),
(25, 'haws-dd', 0),
(26, 'haws-p0', 0),
(27, 'haws-ee', 0),
(28, 'haws-bf', 0),
(29, 'haws-copy', 0),
(30, 'haws-cb', 0),
(31, 'haws-mp', 0),
(32, 'haws-pj', 0),
(33, 'haws-r1', 0),
(34, 'haws-r2', 0),
(35, 'haws-r4', 0),
(36, 'haws-r9', 0),
(37, 'haws-s', 0),
(38, 'haws-ub', 0),
(39, 'haws-um', 0),
(40, 'haws-vb', 0),
(41, 'haws-facnum', 0),
(42, 'haws-funky', 0),
(43, 'haws-imp_exp', 0),
(44, 'haws-incnum', 0),
(45, 'haws-xin', 0),
(46, 'haws-xout', 0),
(47, 'haws-eg', 0),
(48, 'haws-egn', 0),
(49, 'haws-door', 0),
(50, 'b2', 0),
(51, 'b3', 0),
(52, 'b4', 0),
(53, 'b5', 0),
(54, 'haws-gb', 0),
(55, 'haws-gc', 0),
(56, 'haws-invl', 0),
(57, 'haws-invr', 0),
(58, 'haws-lotel', 0),
(59, 'haws-pad', 0),
(60, 'haws-rev', 0),
(61, 'haws-secb', 0),
(62, 'haws-secl', 0),
(63, 'haws-secr', 0),
(64, 'haws-sect', 0),
(65, 'haws-sll', 0),
(66, 'haws-slope', 0),
(67, 'haws-slr', 0),
(68, 'haws-spotel', 0),
(69, 'haws-tc', 0),
(70, 'haws-tcelev', 0),
(71, 'haws-tcelevl', 0),
(72, 'haws-tcelevr', 0),
(73, 'haws-l0', 0),
(74, 'haws-lk0', 0),
(75, 'haws-lka', 0),
(76, 'haws-lki', 0),
(77, 'haws-ofi', 0),
(78, 'haws-ula', 0),
(79, 'haws-laprn', 0),
(80, 'haws-ldr', 0),
(81, 'haws-led', 0),
(82, 'haws-lengthen', 0),
(83, 'haws-lm', 0),
(84, 'haws-loadandrun', 0),
(85, 'haws-m40', 0),
(86, 'haws-m42', 0),
(87, 'haws-mc2033', 0),
(88, 'haws-ffa', 0),
(89, 'haws-hawsalias', 0),
(90, 'haws-pgpedit', 0),
(91, 'haws-user', 0),
(92, 'haws-oo', 0),
(93, 'haws-offsetx', 0),
(94, 'haws-qs14', 0),
(95, 'haws-qs2000', 0),
(96, 'haws-qs2004', 0),
(97, 'haws-pjl', 0),
(98, 'haws-polarset', 0),
(99, 'haws-polaroff', 0),
(102, 'haws-aa', 0),
(103, 'haws-adt', 0),
(104, 'haws-cet', 0),
(105, 'haws-cmd', 0),
(106, 'haws-dia', 0),
(107, 'haws-fdt', 0),
(108, 'haws-mbt', 0),
(109, 'haws-qt', 0),
(110, 'haws-il', 0),
(111, 'haws-io', 0),
(112, 'haws-ir', 0),
(113, 'haws-it', 0),
(114, 'haws-llt', 0),
(115, 'haws-mvl', 0),
(116, 'haws-mvu', 0),
(117, 'haws-ose', 0),
(118, 'haws-osi', 0),
(119, 'haws-osm', 0),
(120, 'haws-osn', 0),
(121, 'haws-pslt', 0),
(122, 'haws-proto', 0),
(123, 'haws-protox', 0),
(124, 'haws-rga', 0),
(125, 'haws-uf', 0),
(126, 'haws-uf0', 0),
(127, 'haws-uf1', 0),
(128, 'haws-vsr', 0),
(129, 'haws-10', 0),
(130, 'haws-12', 0),
(131, 'haws-setdim10', 0),
(132, 'haws-setdim12', 0),
(133, 'haws-setup', 0),
(134, 'haws-sheet', 0),
(135, 'haws-sel', 0),
(136, 'haws-ser', 0),
(137, 'haws-ssx', 0),
(138, 'haws-swap', 0),
(139, 'haws-th', 0),
(140, 'haws-2x', 0),
(141, 'haws-5x', 0),
(142, 'haws-9x', 0),
(143, 'haws-twz', 0),
(144, 'haws-x2', 0),
(145, 'haws-zw', 0),
(146, 'haws-z0', 0),
(147, 'haws-za', 0),
(148, 'haws-ze', 0),
(149, 'haws-zi', 0),
(150, 'haws-zo', 0),
(151, 'haws-zv', 0),
(152, 'haws-zz', 0),
(153, 'haws-2l', 0),
(154, 'haws-add', 1),
(155, 'haws-aee', 0),
(156, 'haws-acres', 0),
(157, 'haws-sf', 0),
(158, 'haws-aet', 0),
(159, 'haws-sm', 0),
(160, 'haws-sy', 0),
(161, 'haws-a2t', 0),
(162, 'haws-att2txt', 0),
(163, 'haws-bdl', 0),
(164, 'haws-bdp', 0),
(165, 'haws-berm', 0),
(166, 'haws-bl0', 0),
(167, 'haws-bw', 0),
(168, 'haws-ca', 1),
(169, 'haws-chattrib', 0),
(170, 'haws-chcoord', 0),
(171, 'haws-chdim', 0),
(172, 'haws-chm', 0),
(173, 'haws-chnum', 0),
(174, 'haws-chgtext', 0),
(175, 'haws-cht', 0),
(176, 'haws-cl', 0),
(177, 'haws-cmpro', 0),
(178, 'haws-cmt', 0),
(179, 'hcnm-cnm', 0),
(180, 'hcnm-cnmkt', 0),
(181, 'hcnm-cnmkti', 0),
(182, '(hcnm-cnm) sub-function', 1),
(183, 'hcnm-linkproj', 0),
(184, 'testset', 0),
(185, 'testget', 0),
(186, 'hcnm-config-setvar', 0),
(187, 'hcnm-config-getvar', 0),
(188, 'hcnm-notesedit', 2),
(189, 'hcnm-cnmlayer', 0),
(190, 'hcnm-setnotesbubblestyle', 0),
(191, 'haws-phaseedit', 0),
(192, 'hcnm-attnoplot', 0),
(193, 'hcnm-attplot', 1),
(194, 'haws-setnotephases', 0),
(195, 'haws-cnmmenu', 0),
(196, 'haws-cnmsetup', 0),
(197, 'haws-ntpurge', 0),
(198, 'haws-boxl', 0),
(199, 'haws-cirl', 0),
(200, 'haws-dial', 0),
(201, 'haws-elll', 0),
(202, 'haws-hexl', 0),
(203, 'haws-octl', 0),
(204, 'haws-penl', 0),
(205, 'haws-recl', 0),
(206, 'haws-sstl', 0),
(207, 'haws-tril', 1),
(208, 'haws-tcg', 0),
(209, 'haws-txtl', 1),
(210, 'hcnm-cnmoptions', 0),
(211, 'haws-contvol', 0),
(212, 'haws-contxt', 0),
(213, 'haws-cs', 1),
(214, 'haws-curve', 0),
(215, 'haws-dw', 0),
(216, 'hcnm-about', 0),
(217, 'haws-about', 0),
(218, 'haws-orderlicenses', 0),
(219, 'haws-icad-p', 0),
(220, 'haws-load-from-app-dir', 0),
(221, 'haws-eop', 0),
(222, 'haws-geodata', 0),
(223, 'haws-goto', 0),
(224, 'haws-incatt', 0),
(226, 'haws-ffi', 0),
(228, 'haws-istan', 0),
(229, 'haws-ff', 0),
(230, 'haws-lk', 0),
(231, 'haws-off', 1),
(232, 'haws-ffx', 0),
(233, 'haws-offx', 1),
(234, 'haws-uff', 0),
(235, 'haws-uffx', 0),
(236, 'haws-uoff', 0),
(237, 'haws-uoffx', 0),
(238, 'haws-las', 0),
(239, 'haws-lar', 0),
(240, 'haws-lcp', 0),
(241, 'haws-lcpx', 0),
(242, 'haws-loop', 0),
(243, 'haws-tilde', 0),
(244, 'haws-dot', 0),
(245, 'haws-none', 0),
(246, 'haws-letter', 0),
(247, 'haws-lotnum', 0),
(248, 'haws-ltc', 0),
(249, 'haws-ltb', 0),
(250, 'haws-lth', 0),
(251, 'haws-ltp', 0),
(252, 'haws-ltpx', 0),
(253, 'haws-lwp', 0),
(254, 'haws-lwpx', 0),
(255, 'haws-lx', 0),
(256, 'haws-lxx', 0),
(257, 'haws-mf', 0),
(258, 'haws-mfillet', 0),
(259, 'haws-mof', 0),
(260, 'haws-moffset', 0),
(261, 'haws-mren', 0),
(262, 'haws-mrename', 0),
(263, 'haws-mscr', 0),
(264, 'haws-mscript', 0),
(265, 'haws-mv', 0),
(266, 'haws-ne', 0),
(267, 'haws-na', 0),
(268, 'haws-newscale', 0),
(269, 'haws-num', 0),
(270, 'haws-pipe', 0),
(271, 'haws-plt', 0),
(272, 'haws-presuf', 0),
(273, 'haws-propipe', 0),
(274, 'haws-prosup', 0),
(275, 'haws-pc', 0),
(276, 'haws-procb', 0),
(277, 'haws-pm', 0),
(278, 'haws-promh', 0),
(279, 'haws-pred', 0),
(280, 'haws-proe', 0),
(281, 'haws-pldr', 0),
(282, 'haws-newpro', 0),
(283, 'haws-profc', 0),
(284, 'haws-pro', 0),
(285, 'haws-tgh2_pro', 0),
(286, 'haws-lst', 0),
(287, 'haws-ellabel', 0),
(288, 'haws-stalabel', 0),
(289, 'haws-elv', 0),
(290, 'haws-grd', 0),
(291, 'haws-grc', 0),
(292, 'haws-grb', 0),
(293, 'haws-pall', 0),
(294, 'haws-l80', 0),
(295, 'haws-l100', 0),
(296, 'haws-l120', 0),
(297, 'haws-l140', 0),
(298, 'haws-l175', 0),
(299, 'haws-l200', 0),
(300, 'haws-l240', 0),
(301, 'haws-l290', 0),
(302, 'haws-l350', 0),
(303, 'haws-l500', 0),
(304, 'haws-rescale', 0),
(305, 'haws-romans', 0),
(306, 'haws-rotatebase', 0),
(307, 'haws-round', 0),
(308, 'haws-ssxpro', 0),
(309, 'haws-stacl', 0),
(310, 'haws-dm', 0),
(311, 'haws-dm12', 0),
(312, 'haws-tap', 0),
(313, 'haws-tapinv', 0),
(314, 'haws-0', 0),
(315, 'haws-1', 0),
(316, 'haws-to', 1),
(317, 'haws-tu', 1),
(318, 'haws-tw', 0),
(319, 'haws-txtsum', 0),
(320, 'haws-u0', 0),
(321, 'haws-u1', 0),
(322, 'haws-u2', 0),
(323, 'haws-u3', 0),
(324, 'haws-u8', 0),
(325, 'haws-us', 0),
(326, 'haws-ut', 0),
(327, 'haws-wall', 0),
(328, 'haws-ws', 0),
(329, 'haws-wl', 0),
(330, 'haws-xd', 0),
(331, 'haws-xro', 0),
(332, 'haws-xroffset', 0),
(333, 'haws-xu', 0),
(334, 'haws-xy', 0),
(335, 'hcnm-notesedit pro', 2),
(336, 'hcnm-cnmqt', 1);

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
  `le_bios_date` varchar(255) DEFAULT NULL,
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

CREATE ALGORITHM=UNDEFINED DEFINER=`jconstru`@`localhost` SQL SECURITY DEFINER VIEW `view_commands_by_count`  AS  select `c`.`command_name` AS `Name`,`c`.`command_id` AS `Id`, sum(`uc`.`uc_count`) AS `Count` from (`command` `c` join `use_count` `uc`) where (`c`.`command_id` = `uc`.`uc_command_id`) group by `c`.`command_name` order by sum(`uc`.`uc_count`) desc ;

-- --------------------------------------------------------

--
-- Structure for view `view_commands_by_logger`
--
DROP TABLE IF EXISTS `view_commands_by_logger`;

CREATE ALGORITHM=UNDEFINED DEFINER=`jconstru`@`localhost` SQL SECURITY DEFINER VIEW `view_commands_by_logger`  AS  select `c`.`command_name` AS `Command name`,`c`.`command_id` AS `Id`,`le`.`le_ip_address` AS `IP`, `le`.`le_computer_name` AS `Computer name`, `le`.`le_bios_date` AS `BIOS_date`, sum(`uc`.`uc_count`) AS `Count` from (`command` `c`, `use_count` `uc`, `log_event` `le`) where (`c`.`command_id` = `uc`.`uc_command_id` AND `uc`.`uc_log_event_id` = `le`.`le_id`) group by `le`.`le_ip_address`, `le`.`le_computer_name`, `le`.`le_bios_date`, `c`.`command_name` order by sum(`uc`.`uc_count`) desc;

-- --------------------------------------------------------

--
-- Structure for view `view_loggers`
--
DROP TABLE IF EXISTS `view_loggers`;

CREATE ALGORITHM=UNDEFINED DEFINER=`jconstru`@`localhost` SQL SECURITY DEFINER VIEW `view_loggers` AS  select `le`.`le_ip_address` AS `IP`, `le`.`le_computer_name` AS `Name`, `le`.`le_bios_date` AS `BIOS_date`, COUNT(`le`.`le_id`) AS `Count` from (`log_event` `le`) group by `le`.`le_ip_address`, `le`.`le_computer_name`, `le`.`le_bios_date` order by `le`.`le_ip_address`, `le`.`le_computer_name`, `le`.`le_bios_date` ;
COMMIT;
-- DELETE uc, le FROM use_count AS uc INNER JOIN log_event AS le ON le.le_id=uc.uc_log_event_id WHERE le.le_computer_name = 'GERANIUM'
