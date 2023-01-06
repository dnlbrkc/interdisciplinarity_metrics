#set up Ubuntu machine

# pip install pyspark
# sudo apt-get update
# sudo apt-get upgrade
# sudo apt-get install openjdk-8-jdk
# sudo apt install dirmngr gnupg apt-transport-https ca-certificates software-properties-common
# sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9sudo add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/'
# sudo apt install r-base

#import Microsoft Academic Graph python notebook
exec(open('magclass.py').read())

#load MAG
MAG = MicrosoftAcademicGraph()


#get journal IDs for Organization Science / Mangement Science & Interdisciplinary journals
List = [ 'Strategic Management Journal','Academy of Management Journal',
'Academy of Management Review',
'Accounting Organizations and Society',
'Administrative Science Quarterly',
'The American Economic Review',
'Econometrica',
'Harvard Business Review',
'Journal of Management',
'Journal of Management Studies',
'Journal of Operations Management',
'Journal of Political Economy',
'Management Science',
'Organization Science',
'Organization Studies',
'Organizational Behavior and Human Decision Processes',
'Quarterly Journal of Economics',
'Research Policy',
'The Review of Economic Studies',
'Review of Finance',
'Sloan Management Review',
'Strategic Entrepreneurship Journal' ,
'Science',
'Science Advances',
"Nature",
"Nature Communications"]

#get journal IDs
journal = MAG.getDataframe( 'Journals' )
journalPapers = journal.filter( journal.DisplayName.isin( List ) ) \
                                 .select(journal.JournalId , 
                                        journal.NormalizedName, 
                                        journal.PaperCount, 
                                        journal.CitationCount)
#journalPapers.show(30)

#get all papers from above journals
papers = MAG.getDataframe( 'Papers' )
IDs = ['206124708',
        '117778295',
        '95464858',
        '3880285',
        '2737427234',
        '137773608',
        '64187185',
        '9731383',
        '28882882',
        '189923312',
        '46763546',
        '31690342',
        '88935262',
        '131117787',
        '151705444',
        '122767448',
        '102949365',
        '95323914',
        '4146626',
        '142306484',
        '203860005',
        '198892436',
        '143668711',
        '33323087',
        '64744539',
        '23254222',
        '137773608']

jp = papers.filter( papers.JournalId.isin(IDs)) \
                                 .select(papers.JournalId , 
                                        papers.PaperId, 
                                        papers.PaperTitle, 
                                        papers.CitationCount,
                                        papers.Year)

#jp.show(10)


#get references for each paper above
refs= MAG.getDataframe( 'PaperReferences' )
paperCitations = refs.join( jp , jp.PaperId == refs.PaperId)
newNames = ['PaperId','PaperReferenceId', 'JournalId','PaperId2', 'PaperTitle','CitationCount','Year']
paperCitations2 = paperCitations.toDF(*newNames)

paperCitations2.write.csv("PapersJournals")

#get journal info for references cited by papers 

papers =  MAG.getDataframe( 'Papers' )
refJournals = papers.join( paperCitations2 , paperCitations2.PaperReferenceId == papers.PaperId) \
                    .select(paperCitations2.PaperId,paperCitations2.PaperReferenceId,papers.JournalId,papers.Year)

#write output
refJournals.write.csv("ReferencesJournals")
