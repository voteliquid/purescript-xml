module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (Either(..))
import Data.XML (parseXML)
import Text.Parsing.Parser (parseErrorMessage, parseErrorPosition)

main :: ∀ e. Eff (console :: CONSOLE | e) Unit
main =
  case parseXML testXML of
    Left err -> do
      log (parseErrorMessage err)
      log ("position: " <> show (parseErrorPosition err))
    Right xml -> pure unit

testXML :: String
testXML = """<?xml version="1.0"?>
<billStatus>
  <bill>
    <policyArea>
      <name>Congress</name>
    </policyArea>
    <actions>
      <item>
        <type>Floor</type>
        <committee />
        <actionTime>14:47:48</actionTime>
        <actionCode>H38310</actionCode>
        <actionDate>2017-02-14</actionDate>
        <text>Motion to reconsider laid on the table Agreed to without objection.</text>
        <links />
        <sourceSystem>
          <code>2</code>
          <name>House floor actions</name>
        </sourceSystem>
      </item>
      <item>
        <type>Floor</type>
        <committee />
        <actionTime>14:47:46</actionTime>
        <actionCode>H37100</actionCode>
        <actionDate>2017-02-14</actionDate>
        <text>On agreeing to the resolution Agreed to by recorded vote: 225 - 187 (Roll no. 89). (text: CR H1145)</text>
        <links>
          <link>
            <url>https://www.congress.gov/congressional-record/volume-163/house-section/page/H1145</url>
            <name>H1145</name>
          </link>
          <link>
            <url>http://clerk.house.gov/evs/2017/roll089.xml</url>
            <name>Roll no. 89</name>
          </link>
        </links>
        <sourceSystem>
          <code>2</code>
          <name>House floor actions</name>
        </sourceSystem>
      </item>
      <item>
        <type>Floor</type>
        <committee />
        <actionTime>14:47:46</actionTime>
        <actionCode>8000</actionCode>
        <actionDate>2017-02-14</actionDate>
        <text>Passed/agreed to in House: On agreeing to the resolution Agreed to by recorded vote: 225 - 187 (Roll no. 89).(text: CR H1145)</text>
        <links>
          <link>
            <url>https://www.congress.gov/congressional-record/volume-163/house-section/page/H1145</url>
            <name>H1145</name>
          </link>
          <link>
            <url>http://clerk.house.gov/evs/2017/roll089.xml</url>
            <name>Roll no. 89</name>
          </link>
        </links>
        <sourceSystem>
          <code>9</code>
          <name>Library of Congress</name>
        </sourceSystem>
      </item>
      <item>
        <type>Floor</type>
        <committee />
        <actionTime>14:41:11</actionTime>
        <actionCode>H35000</actionCode>
        <actionDate>2017-02-14</actionDate>
        <text>On ordering the previous question Agreed to by the Yeas and Nays: 225 - 189 (Roll no. 88). (consideration: CR H1151-1152)</text>
        <links>
          <link>
            <url>https://www.congress.gov/congressional-record/volume-163/house-section/page/H1151-1152</url>
            <name>H1151-1152</name>
          </link>
          <link>
            <url>http://clerk.house.gov/evs/2017/roll088.xml</url>
            <name>Roll no. 88</name>
          </link>
        </links>
        <sourceSystem>
          <code>2</code>
          <name>House floor actions</name>
        </sourceSystem>
      </item>
      <item>
        <type>Floor</type>
        <committee />
        <actionTime>14:16:15</actionTime>
        <actionCode>H30000</actionCode>
        <actionDate>2017-02-14</actionDate>
        <text>Considered as unfinished business. (consideration: CR H1151-1152)</text>
        <links>
          <link>
            <url>https://www.congress.gov/congressional-record/volume-163/house-section/page/H1151-1152</url>
            <name>H1151-1152</name>
          </link>
        </links>
        <sourceSystem>
          <code>2</code>
          <name>House floor actions</name>
        </sourceSystem>
      </item>
      <item>
        <type>Floor</type>
        <committee />
        <actionTime>13:55:51</actionTime>
        <actionCode>H8D000</actionCode>
        <actionDate>2017-02-14</actionDate>
        <text>POSTPONED PROCEEDINGS - At the conclusion of debate on H. Res. 99, the Chair put the question on ordering the previous question and by voice vote announced that the ayes had prevailed. Mr. Hastings demanded the yeas and nays, and the Chair postponed further proceedings on ordering the previous question until later in the legislative day.</text>
        <links>
          <link>
            <url>https://www.congress.gov/bill/115th-congress/house-resolution/99</url>
            <name>H. Res. 99</name>
          </link>
        </links>
        <sourceSystem>
          <code>2</code>
          <name>House floor actions</name>
        </sourceSystem>
      </item>
      <item>
        <type>Floor</type>
        <committee />
        <actionTime>13:12:34</actionTime>
        <actionCode>H8D000</actionCode>
        <actionDate>2017-02-14</actionDate>
        <text>DEBATE - The House proceeded with one hour of debate on H. Res. 99.</text>
        <links>
          <link>
            <url>https://www.congress.gov/bill/115th-congress/house-resolution/99</url>
            <name>H. Res. 99</name>
          </link>
        </links>
        <sourceSystem>
          <code>2</code>
          <name>House floor actions</name>
        </sourceSystem>
      </item>
      <item>
        <type>Floor</type>
        <committee />
        <actionTime>13:10:52</actionTime>
        <actionCode>H30000</actionCode>
        <actionDate>2017-02-14</actionDate>
        <text>Considered as privileged matter. (consideration: CR H1145-1151)</text>
        <links>
          <link>
            <url>https://www.congress.gov/congressional-record/volume-163/house-section/page/H1145-1151</url>
            <name>H1145-1151</name>
          </link>
        </links>
        <sourceSystem>
          <code>2</code>
          <name>House floor actions</name>
        </sourceSystem>
      </item>
      <item>
        <type>Calendars</type>
        <committee />
        <actionCode>H12420</actionCode>
        <actionDate>2017-02-07</actionDate>
        <text>Placed on the House Calendar, Calendar No. 10.</text>
        <links />
        <sourceSystem>
          <code>2</code>
          <name>House floor actions</name>
        </sourceSystem>
      </item>
      <item>
        <type>Floor</type>
        <committee />
        <actionTime>17:20:40</actionTime>
        <actionCode>H12700</actionCode>
        <actionDate>2017-02-07</actionDate>
        <text>Rule provides for one hour of debate on each bill, the previous question shall be ordered on each bill without intervening motions except one motion to recommit. Both bills are closed to amendments.</text>
        <links />
        <sourceSystem>
          <code>2</code>
          <name>House floor actions</name>
        </sourceSystem>
      </item>
      <item>
        <type>Committee</type>
        <committee>
          <name>Rules Committee</name>
          <systemCode>hsru00</systemCode>
        </committee>
        <actionCode>H12100</actionCode>
        <actionDate>2017-02-07</actionDate>
        <text>The House Committee on Rules reported an original measure, H. Rept. 115-10, by Mr. Cole.</text>
        <links>
          <link>
            <url>https://www.congress.gov/congressional-report/115th-congress/house-report/10</url>
            <name>H. Rept. 115-10</name>
          </link>
        </links>
        <sourceSystem>
          <code>2</code>
          <name>House floor actions</name>
        </sourceSystem>
      </item>
      <item>
        <type>Committee</type>
        <committee>
          <name>Rules Committee</name>
          <systemCode>hsru00</systemCode>
        </committee>
        <actionCode>5000</actionCode>
        <actionDate>2017-02-07</actionDate>
        <text>The House Committee on Rules reported an original measure, H. Rept. 115-10, by Mr. Cole.</text>
        <links>
          <link>
            <url>https://www.congress.gov/congressional-report/115th-congress/house-report/10</url>
            <name>H. Rept. 115-10</name>
          </link>
        </links>
        <sourceSystem>
          <code>9</code>
          <name>Library of Congress</name>
        </sourceSystem>
      </item>
      <item>
        <type>IntroReferral</type>
        <committee />
        <actionCode>1000</actionCode>
        <actionDate>2017-02-07</actionDate>
        <text>Introduced in House</text>
        <links />
        <sourceSystem>
          <code>9</code>
          <name>Library of Congress</name>
        </sourceSystem>
      </item>
      <actionTypeCounts>
        <houseCalendarAssignment>1</houseCalendarAssignment>
        <motionForPreviousQuestion>1</motionForPreviousQuestion>
        <provisionsOfARule>1</provisionsOfARule>
        <motionToReconsiderResults>1</motionToReconsiderResults>
        <passedAgreedToInHouse>1</passedAgreedToInHouse>
        <considerationByHouse>2</considerationByHouse>
        <passageOfAMeasure>1</passageOfAMeasure>
        <generalDebate>2</generalDebate>
        <committeeReportOfAnOriginalMeasure>1</committeeReportOfAnOriginalMeasure>
        <introducedInHouse>1</introducedInHouse>
        <reportedToHouse>1</reportedToHouse>
      </actionTypeCounts>
      <actionByCounts>
        <houseOfRepresentatives>13</houseOfRepresentatives>
      </actionByCounts>
    </actions>
    <recordedVotes>
      <recordedVote>
        <date>2017-02-14T19:47:46Z</date>
        <chamber>House</chamber>
        <url>http://clerk.house.gov/evs/2017/roll089.xml</url>
        <sessionNumber>1</sessionNumber>
        <rollNumber>89</rollNumber>
        <congress>115</congress>
        <fullActionName>Passage of a Measure</fullActionName>
      </recordedVote>
      <recordedVote>
        <date>2017-02-14T19:41:11Z</date>
        <chamber>House</chamber>
        <url>http://clerk.house.gov/evs/2017/roll088.xml</url>
        <sessionNumber>1</sessionNumber>
        <rollNumber>88</rollNumber>
        <congress>115</congress>
        <fullActionName>Motion for Previous Question</fullActionName>
      </recordedVote>
    </recordedVotes>
    <amendments />
    <introducedDate>2017-02-07</introducedDate>
    <updateDate>2017-05-04T00:16:58Z</updateDate>
    <laws />
    <cboCostEstimates />
    <originChamber>House</originChamber>
    <createDate>2017-02-08T04:45:55Z</createDate>
    <subjects>
      <billSubjects>
        <legislativeSubjects>
          <item>
            <name>House of Representatives</name>
          </item>
          <item>
            <name>Legislative rules and procedure</name>
          </item>
        </legislativeSubjects>
        <policyArea>
          <name>Congress</name>
        </policyArea>
      </billSubjects>
    </subjects>
    <notes />
    <latestAction>
      <links />
      <actionDate>2017-02-14</actionDate>
      <text>Motion to reconsider laid on the table Agreed to without objection.</text>
      <actionTime>14:47:48</actionTime>
    </latestAction>
    <version>1.0.0</version>
    <constitutionalAuthorityStatementText />
    <summaries>
      <billSummaries>
        <item>
          <actionDate>2017-02-07</actionDate>
          <lastSummaryUpdateDate>2017-02-08T09:02:58Z</lastSummaryUpdateDate>
          <actionDesc>Introduced in House</actionDesc>
          <updateDate>2017-02-07T05:00:00Z</updateDate>
          <text><![CDATA[Sets forth the rule for consideration of the bill (H.R. 428) to survey the gradient boundary along the Red River in the States of Oklahoma and Texas, and for other purposes, and providing for consideration of the joint resolution (H.J. Res. 42) disapproving the rule submitted by the Department of Labor relating to drug testing of unemployment compensation applicants.]]></text>
          <versionCode>00</versionCode>
          <name>Introduced in House</name>
        </item>
      </billSummaries>
    </summaries>
    <title>Providing for consideration of the bill (H.R. 428) to survey the gradient boundary along the Red River in the States of Oklahoma and Texas, and for other purposes, and providing for consideration of the joint resolution (H.J. Res. 42) disapproving the rule submitted by the Department of Labor relating to drug testing of unemployment compensation applicants.</title>
    <committees>
      <billCommittees>
        <item>
          <type>Standing</type>
          <subcommittees />
          <activities>
            <item>
              <name>Reported original measure</name>
              <date>2017-02-07T22:20:39Z</date>
            </item>
          </activities>
          <chamber>House</chamber>
          <name>Rules Committee</name>
          <systemCode>hsru00</systemCode>
        </item>
      </billCommittees>
    </committees>
    <billType>HRES</billType>
    <congress>115</congress>
    <titles>
      <item>
        <titleType>Official Title as Introduced</titleType>
        <chamberName />
        <chamberCode />
        <title>Providing for consideration of the bill (H.R. 428) to survey the gradient boundary along the Red River in the States of Oklahoma and Texas, and for other purposes, and providing for consideration of the joint resolution (H.J. Res. 42) disapproving the rule submitted by the Department of Labor relating to drug testing of unemployment compensation applicants.</title>
        <parentTitleType />
      </item>
      <item>
        <titleType>Display Title</titleType>
        <chamberName />
        <chamberCode />
        <title>Providing for consideration of the bill (H.R. 428) to survey the gradient boundary along the Red River in the States of Oklahoma and Texas, and for other purposes, and providing for consideration of the joint resolution (H.J. Res. 42) disapproving the rule submitted by the Department of Labor relating to drug testing of unemployment compensation applicants.</title>
        <parentTitleType />
      </item>
    </titles>
    <relatedBills>
      <item>
        <type>HR</type>
        <number>428</number>
        <relationshipDetails>
          <item>
            <type>Related bill</type>
            <identifiedBy>House</identifiedBy>
          </item>
          <item>
            <type>Procedurally-related</type>
            <identifiedBy>House</identifiedBy>
          </item>
        </relationshipDetails>
        <latestTitle>Red River Gradient Boundary Survey Act</latestTitle>
        <latestAction>
          <actionDate>2017-03-07</actionDate>
          <text>Read twice and referred to the Committee on Energy and Natural Resources.</text>
        </latestAction>
        <congress>115</congress>
      </item>
      <item>
        <type>HJRES</type>
        <number>42</number>
        <relationshipDetails>
          <item>
            <type>Procedurally-related</type>
            <identifiedBy>House</identifiedBy>
          </item>
        </relationshipDetails>
        <latestTitle>Disapproving the rule submitted by the Department of Labor relating to drug testing of unemployment compensation applicants.</latestTitle>
        <latestAction>
          <actionDate>2017-03-31</actionDate>
          <text>Became Public Law No: 115-17.</text>
        </latestAction>
        <congress>115</congress>
      </item>
    </relatedBills>
    <committeeReports>
      <committeeReport>
        <citation>H. Rept. 115-10</citation>
      </committeeReport>
    </committeeReports>
    <sponsors>
      <item>
        <state>OK</state>
        <party>R</party>
        <district>4</district>
        <identifiers>
          <lisID>1742</lisID>
          <gpoId>8112</gpoId>
          <bioguideId>C001053</bioguideId>
        </identifiers>
        <lastName>Cole</lastName>
        <firstName>Tom</firstName>
        <fullName>Rep. Cole, Tom [R-OK-4]</fullName>
        <bioguideId>C001053</bioguideId>
        <byRequestType />
        <middleName />
      </item>
    </sponsors>
    <billNumber>99</billNumber>
    <cosponsors />
    <calendarNumbers>
      <item>
        <calendar>H00010</calendar>
        <number />
      </item>
    </calendarNumbers>
  </bill>
  <dublinCore xmlns:dc="http://purl.org/dc/elements/1.1/">
    <dc:format>text/xml</dc:format>
    <dc:language>EN</dc:language>
    <dc:rights>Pursuant to Title 17 Section 105 of the United States Code, this file is not subject to copyright protection and is in the public domain.</dc:rights>
    <dc:contributor>Congressional Research Service, Library of Congress</dc:contributor>
    <dc:description>This file contains bill summaries and statuses for federal legislation. A bill summary describes the most significant provisions of a piece of legislation and details the effects the legislative text may have on current law and federal programs. Bill summaries are authored by the Congressional Research Service (CRS) of the Library of Congress. As stated in Public Law 91-510 (2 USC 166 (d)(6)), one of the duties of CRS is "to prepare summaries and digests of bills and resolutions of a public general nature introduced in the Senate or House of Representatives". For more information, refer to the User Guide that accompanies this file.</dc:description>
  </dublinCore>
</billStatus>
"""
