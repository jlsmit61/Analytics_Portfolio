SELECT 
	
	   SUBSTRING(column2, CHARINDEX('<', column2) + 1, CHARINDEX('.', column2)) as Message_ID,
	   SUBSTRING(column2, CHARINDEX(',', column2) + 1, CHARINDEX(' ', column2) + 9) as Date_time,
	   SUBSTRING(column2, CHARINDEX('From: ', column2) + 6, CHARINDEX('m', SUBSTRING(column2, CHARINDEX('From: ', column2, 0) + 6, LEN(CONVERT(varchar,column2))), 0)) as From_Email,
	   SUBSTRING(column2, CHARINDEX('To: ', column2) + 4, CHARINDEX('com', substring(column2, charindex('To: ', column2, 0) + 4, LEN(CONVERT(varchar,column2))), 0) + 2) as To_Email,	
	   REPLACE(SUBSTRING(column2, CHARINDEX('Subject', column2) + 8, CHARINDEX(' ', substring(column2, charindex('Subject', column2, 0) + 8, LEN(CONVERT(varchar,column2))), 24)+ 30), 'Mime-', '') as Email_Subject,
	   REPLACE(SUBSTRING(column2, CHARINDEX('X-From: ', column2) + 8, CHARINDEX('X', substring(column2, charindex('X-From: ', column2, 0)  +8, LEN(CONVERT(varchar,column2))), 0)), 'X', ' ') as Sender,
	  CASE 
			WHEN REPLACE(SUBSTRING(column2, CHARINDEX('X-To:', column2, 0) + 5, CHARINDEX(' ', substring(column2, charindex('X-To:', column2, 0) + 5, LEN(CONVERT(varchar, column2))), 10)), 'X-cc:', '') =  ' '
				THEN SUBSTRING(column2, CHARINDEX('To: ', column2) + 4, CHARINDEX('com', substring(column2, charindex('To: ', column2, 0) + 4, LEN(CONVERT(varchar,column2))), 0) + 2)
			WHEN SUBSTRING(column2, CHARINDEX('To: ', column2) + 4, CHARINDEX('com', substring(column2, charindex('To: ', column2, 0) + 4, LEN(CONVERT(varchar,column2))), 0) + 2) = 'jennifer.burns@enron.com'
				THEN 'Jennifer Burns'
			WHEN SUBSTRING(column2, CHARINDEX('To: ', column2) + 4, CHARINDEX('com', substring(column2, charindex('To: ', column2, 0) + 4, LEN(CONVERT(varchar,column2))), 0) + 2) = 'jennifer.fraser@enron.com'
				THEN 'Jennifer Fraser'
			WHEN SUBSTRING(column2, CHARINDEX('To: ', column2) + 4, CHARINDEX('com', substring(column2, charindex('To: ', column2, 0) + 4, LEN(CONVERT(varchar,column2))), 0) + 2) = 'vladimir.gorny@enron.com'
				THEN 'Vladmir Gorny'
			WHEN SUBSTRING(column2, CHARINDEX('To: ', column2) + 4, CHARINDEX('com', substring(column2, charindex('To: ', column2, 0) + 4, LEN(CONVERT(varchar,column2))), 0) + 2) = 'margaret.allen@enron.com'
				THEN 'Margaret Allen'
			WHEN SUBSTRING(column2, CHARINDEX('To: ', column2) + 4, CHARINDEX('com', substring(column2, charindex('To: ', column2, 0) + 4, LEN(CONVERT(varchar,column2))), 0) + 2) = 'kimberly.hillis@enron.com'
				THEN 'Kimberly Hillis'
			WHEN SUBSTRING(column2, CHARINDEX('To: ', column2) + 4, CHARINDEX('com', substring(column2, charindex('To: ', column2, 0) + 4, LEN(CONVERT(varchar,column2))), 0) + 2) = 'leah.arsdall@enron.com'
				THEN 'Leah Van Arsdall'
			WHEN SUBSTRING(column2, CHARINDEX('To: ', column2) + 4, CHARINDEX('com', substring(column2, charindex('To: ', column2, 0) + 4, LEN(CONVERT(varchar,column2))), 0) + 2) = 'randall.gay@enron.com'
				THEN 'Randall L Gay'
			WHEN SUBSTRING(column2, CHARINDEX('To: ', column2) + 4, CHARINDEX('com', substring(column2, charindex('To: ', column2, 0) + 4, LEN(CONVERT(varchar,column2))), 0) + 2) = 'jeffrey.gossett@enron.com'
				THEN 'Jeffrey C Gossett'
			WHEN SUBSTRING(column2, CHARINDEX('To: ', column2) + 4, CHARINDEX('com', substring(column2, charindex('To: ', column2, 0) + 4, LEN(CONVERT(varchar,column2))), 0) + 2) = 'hunter.shively@enron.com'
				THEN 'Hunter S Shively'
			WHEN SUBSTRING(column2, CHARINDEX('To: ', column2) + 4, CHARINDEX('com', substring(column2, charindex('To: ', column2, 0) + 4, LEN(CONVERT(varchar,column2))), 0) + 2) = 'stephanie.sever@enron.com'
				THEN 'Stephanie Sever'
			WHEN SUBSTRING(column2, CHARINDEX('To: ', column2) + 4, CHARINDEX('com', substring(column2, charindex('To: ', column2, 0) + 4, LEN(CONVERT(varchar,column2))), 0) + 2) = 'jeffrey.hodge@enron.com'
				THEN 'Jeffrey T Hodge'
			WHEN SUBSTRING(column2, CHARINDEX('To: ', column2) + 4, CHARINDEX('com', substring(column2, charindex('To: ', column2, 0) + 4, LEN(CONVERT(varchar,column2))), 0) + 2) = 'yvette.connevey@enron.com'
				THEN 'Yvette G Connevey' 
			WHEN SUBSTRING(column2, CHARINDEX('To: ', column2) + 4, CHARINDEX('com', substring(column2, charindex('To: ', column2, 0) + 4, LEN(CONVERT(varchar,column2))), 0) + 2) = 'phillip.love@enron.com'
				THEN 'Phillip Love'
			WHEN SUBSTRING(column2, CHARINDEX('To: ', column2) + 4, CHARINDEX('com', substring(column2, charindex('To: ', column2, 0) + 4, LEN(CONVERT(varchar,column2))), 0) + 2) = 'howard.camp@enron.com'
				THEN 'Howard Camp'
			WHEN SUBSTRING(column2, CHARINDEX('To: ', column2) + 4, CHARINDEX('com', substring(column2, charindex('To: ', column2, 0) + 4, LEN(CONVERT(varchar,column2))), 0) + 2) = 'o''neal.winfree@enron.com'
				THEN 'O''Neal D Winfree'
			WHEN SUBSTRING(column2, CHARINDEX('To: ', column2) + 4, CHARINDEX('com', substring(column2, charindex('To: ', column2, 0) + 4, LEN(CONVERT(varchar,column2))), 0) + 2) = 'benjamin.markey@enron.com'
				THEN 'Benjamin Markey'
			WHEN SUBSTRING(column2, CHARINDEX('To: ', column2) + 4, CHARINDEX('com', substring(column2, charindex('To: ', column2, 0) + 4, LEN(CONVERT(varchar,column2))), 0) + 2) = 'brenda.herod@enron.com'
				THEN 'Brenda F Herod'
			WHEN SUBSTRING(column2, CHARINDEX('To: ', column2) + 4, CHARINDEX('com', substring(column2, charindex('To: ', column2, 0) + 4, LEN(CONVERT(varchar,column2))), 0) + 2) = 'edward.gottlob@enron.com'
				THEN 'Edward D Gottlob'
			WHEN SUBSTRING(column2, CHARINDEX('To: ', column2) + 4, CHARINDEX('com', substring(column2, charindex('To: ', column2, 0) + 4, LEN(CONVERT(varchar,column2))), 0) + 2) = 'danielles@jonesgranger.com'
				THEN 'danielles@jonesgranger.com'
			WHEN SUBSTRING(column2, CHARINDEX('To: ', column2) + 4, CHARINDEX('com', substring(column2, charindex('To: ', column2, 0) + 4, LEN(CONVERT(varchar,column2))), 0) + 2) = 'elizabeth.boudreaux@enron.com'
				THEN 'Elizabeth Boudreaux'
			WHEN SUBSTRING(column2, CHARINDEX('To: ', column2) + 4, CHARINDEX('com', substring(column2, charindex('To: ', column2, 0) + 4, LEN(CONVERT(varchar,column2))), 0) + 2) = 'thomas.martin@enron.com'
				THEN 'Thomas A Martin'
			WHEN SUBSTRING(column2, CHARINDEX('To: ', column2) + 4, CHARINDEX('com', substring(column2, charindex('To: ', column2, 0) + 4, LEN(CONVERT(varchar,column2))), 0) + 2) = 'gwendolyn.williams@enron.com'
				THEN 'Gwendolyn Williams'
			WHEN SUBSTRING(column2, CHARINDEX('To: ', column2) + 4, CHARINDEX('com', substring(column2, charindex('To: ', column2, 0) + 4, LEN(CONVERT(varchar,column2))), 0) + 2) = 'robert.cass@enron.com'
				THEN 'Robert B Cass'
			WHEN SUBSTRING(column2, CHARINDEX('To: ', column2) + 4, CHARINDEX('com', substring(column2, charindex('To: ', column2, 0) + 4, LEN(CONVERT(varchar,column2))), 0) + 2) = 'phillip.allen@enron.com'
				THEN 'Phillip K Allen'
			WHEN SUBSTRING(column2, CHARINDEX('To: ', column2) + 4, CHARINDEX('com', substring(column2, charindex('To: ', column2, 0) + 4, LEN(CONVERT(varchar,column2))), 0) + 2) = 'catherine.pernot@enron.com'
				THEN 'Catherine Pernot'
			WHEN SUBSTRING(column2, CHARINDEX('To: ', column2) + 4, CHARINDEX('com', substring(column2, charindex('To: ', column2, 0) + 4, LEN(CONVERT(varchar,column2))), 0) + 2) = 'jeffrey.shankman@enron.com'
				THEN 'Jeffrey A Shankman'
			WHEN SUBSTRING(column2, CHARINDEX('To: ', column2) + 4, CHARINDEX('com', substring(column2, charindex('To: ', column2, 0) + 4, LEN(CONVERT(varchar,column2))), 0) + 2) = 'thomas.white@enron.com'
				THEN 'Thomas E White'
			WHEN SUBSTRING(column2, CHARINDEX('To: ', column2) + 4, CHARINDEX('com', substring(column2, charindex('To: ', column2, 0) + 4, LEN(CONVERT(varchar,column2))), 0) + 2) = 'caroline.abramo@enron.com'
				THEN 'Caroline Abramo'
			WHEN SUBSTRING(column2, CHARINDEX('To: ', column2) + 4, CHARINDEX('com', substring(column2, charindex('To: ', column2, 0) + 4, LEN(CONVERT(varchar,column2))), 0) + 2) = 'sandra.brawner@enron.com'
				THEN 'Sandra F Brawner'
			ELSE REPLACE(SUBSTRING(column2, CHARINDEX('X-To:', column2, 0) + 5, CHARINDEX(' ', substring(column2, charindex('X-To:', column2, 0) + 5, LEN(CONVERT(varchar, column2))), 10)), 'X-cc:', '')
		END as Receiver,
	 REPLACE(SUBSTRING(column2, CHARINDEX('X-cc:', column2) + 6, CHARINDEX(':', substring(column2, charindex('X-cc:', column2, 0)  +6, LEN(CONVERT(varchar,column2))), 0)), 'X-bcc:', SPACE(1)) as CC,
	 REPLACE(SUBSTRING(column2, CHARINDEX('X-bcc:', column2) + 5, CHARINDEX(':', substring(column2, charindex('X-bcc', column2, 0) + 5, LEN(CONVERT(varchar,column2))), 0)), ':', SPACE(1)) as BCC,
 	 LTRIM(REPLACE(SUBSTRING(column2, CHARINDEX('X-FileName', column2, 0) + 23, LEN(CONVERT(varchar(8000), column2))), '-Privileged).pst', '')) as Message_Body

  FROM [Enron].[dbo].[ENRON_EMAILS]