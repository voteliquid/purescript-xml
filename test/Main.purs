module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.XML (XML(..), parseXML)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual, fail)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Text.Parsing.Parser (parseErrorPosition)
import Text.Parsing.Parser.Pos (Position(..))

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "parseXML" do
    it "should parse a full xml test file successfully" do
      case parseXML testXML of
        Left err -> fail "parsing failed"
        Right xml -> pure unit

    it "should parse a simple xml file" do
      let example = parseXML """<?xml version="1.0" encoding="UTF-8"?><my-test>my-text</my-test>"""
      let expected =  XMLNode "my-test" mempty $ pure $ XMLContent "my-text"
      example `shouldEqual` Right expected
      
    it "should report an error for a malformed xml file" do
      let example = parseXML """<?xml version="1.0" encoding="UTF-8"?><"""
      case example of
        Left err -> parseErrorPosition err `shouldEqual` (Position {line: 1, column: 40})
        Right xml -> fail "expected xml to be invalid, but parseXML did not fail"

testXML :: String
testXML = """
<?xml version="1.0" encoding="UTF-8"?>
<billStatus>
  <bill>
    <createDate>2017-01-04T07:01:10Z</createDate>
    <actions>
      <actionTypeCounts>
        <billReferrals>2</billReferrals>
        <placeholderTextForH>1</placeholderTextForH>
        <introducedInHouse>1</introducedInHouse>
        <introducedInTheHouse>1</introducedInTheHouse>
        <billReferralsAggregate>1</billReferralsAggregate>
      </actionTypeCounts>
      <item>
        <actionDate>2017-01-12</actionDate>
        <committee>
          <systemCode>hsju01</systemCode>
          <name>Immigration and Border Security Subcommittee</name>
        </committee>
        <links />
        <sourceSystem>
          <code>1</code>
          <name>House committee actions</name>
        </sourceSystem>
        <text>Referred to the Subcommittee on Immigration and Border Security.</text>
        <type>Committee</type>
      </item>
      <item>
        <links />
        <type>IntroReferral</type>
        <actionDate>2017-01-03</actionDate>
        <committee>
          <systemCode>hsju00</systemCode>
          <name>Judiciary Committee</name>
        </committee>
        <text>Referred to House Judiciary</text>
        <actionCode>H11100</actionCode>
        <sourceSystem>
          <code>2</code>
          <name>House floor actions</name>
        </sourceSystem>
      </item>
      <item>
        <links />
        <type>IntroReferral</type>
        <actionDate>2017-01-03</actionDate>
        <committee>
          <systemCode>hsfa00</systemCode>
          <name>Foreign Affairs Committee</name>
        </committee>
        <text>Referred to House Foreign Affairs</text>
        <actionCode>H11100</actionCode>
        <sourceSystem>
          <code>2</code>
          <name>House floor actions</name>
        </sourceSystem>
      </item>
      <item>
        <links />
        <type>IntroReferral</type>
        <actionDate>2017-01-03</actionDate>
        <committee />
        <text>Referred to the Committee on Foreign Affairs, and in addition to the Committee on the Judiciary, for a period to be subsequently determined by the Speaker, in each case for consideration of such provisions as fall within the jurisdiction of the committee concerned.</text>
        <actionCode>H11100-A</actionCode>
        <sourceSystem>
          <code>9</code>
          <name>Library of Congress</name>
        </sourceSystem>
      </item>
      <item>
        <links />
        <type>IntroReferral</type>
        <actionDate>2017-01-03</actionDate>
        <committee />
        <text>Introduced in House</text>
        <actionCode>Intro-H</actionCode>
        <sourceSystem>
          <code>9</code>
          <name>Library of Congress</name>
        </sourceSystem>
      </item>
      <item>
        <links />
        <type>IntroReferral</type>
        <actionDate>2017-01-03</actionDate>
        <committee />
        <text>Introduced in House</text>
        <actionCode>1000</actionCode>
        <sourceSystem>
          <code>9</code>
          <name>Library of Congress</name>
        </sourceSystem>
      </item>
      <actionByCounts>
        <houseOfRepresentatives>6</houseOfRepresentatives>
      </actionByCounts>
    </actions>
    <constitutionalAuthorityStatementText><![CDATA[<pre>From the Congressional Record Online through the Government Publishing Office [<a href='http://www.gpo.gov'>www.gpo.gov</a>]By Mr. BABIN:H.R. 82.Congress has the power to enact this legislation pursuantto the following:Article I, Section 8, Clause 4Article I, Section 8, Clause 18[Page H49]</pre>]]></constitutionalAuthorityStatementText>
    <cboCostEstimates />
    <policyArea>
      <name>International Affairs</name>
    </policyArea>
    <title>Criminal Alien Deportation Enforcement Act of 2017</title>
    <laws />
    <sponsors>
      <item>
        <byRequestType />
        <middleName />
        <district>36</district>
        <state>TX</state>
        <identifiers>
          <bioguideId>B001291</bioguideId>
          <lisID>2270</lisID>
          <gpoId />
        </identifiers>
        <bioguideId>B001291</bioguideId>
        <fullName>Rep. Babin, Brian [R-TX-36]</fullName>
        <firstName>Brian</firstName>
        <party>R</party>
        <lastName>Babin</lastName>
      </item>
    </sponsors>
    <latestAction>
      <text>Referred to the Subcommittee on Immigration and Border Security.</text>
      <actionDate>2017-01-12</actionDate>
      <links />
    </latestAction>
    <committees>
      <billCommittees>
        <item>
          <systemCode>hsju00</systemCode>
          <chamber>House</chamber>
          <type>Standing</type>
          <name>Judiciary Committee</name>
          <subcommittees>
            <item>
              <name>Immigration and Border Security Subcommittee</name>
              <activities>
                <item>
                  <name>Referred to</name>
                  <date>2017-01-12T16:00:23Z</date>
                </item>
              </activities>
              <systemCode>hsju01</systemCode>
            </item>
          </subcommittees>
          <activities>
            <item>
              <name>Referred to</name>
              <date>2017-01-03T17:17:25Z</date>
            </item>
          </activities>
        </item>
        <item>
          <systemCode>hsfa00</systemCode>
          <chamber>House</chamber>
          <type>Standing</type>
          <name>Foreign Affairs Committee</name>
          <subcommittees />
          <activities>
            <item>
              <name>Referred to</name>
              <date>2017-01-03T17:17:20Z</date>
            </item>
          </activities>
        </item>
      </billCommittees>
    </committees>
    <summaries>
      <billSummaries>
        <item>
          <updateDate>2017-01-03T05:00:00Z</updateDate>
          <lastSummaryUpdateDate>2017-01-12T16:32:53Z</lastSummaryUpdateDate>
          <actionDate>2017-01-03</actionDate>
          <text><![CDATA[<p><strong>Criminal Alien Deportation Enforcement Act of 2017</strong></p> <p>This bill amends the Foreign Assistance Act of 1961 to prohibit financial assistance to a foreign country that refuses or unreasonably delays the acceptance of an alien who: (1) is a citizen, subject, national, or resident of such country; and (2) has received a final order of removal from the United States.</p> <p>A country shall be deemed to have refused or unreasonably delayed acceptance of an alien if it does not accept such alien within 90 days of receiving an authorized repatriation request. </p> <p>The Department of Homeland Security shall submit a report to Congress every three months that: (1) lists the countries that refuse or unreasonably delay repatriation; and (2) includes the total number of aliens who were refused repatriation, organized by country, detention status, and criminal status.</p> <p> A listed country shall be subject to U.S. entry and financial assistance prohibitions unless it issues appropriate travel documents: (1) within 100 days after such report's submission for aliens convicted of a crime committed in the United States, and (2) within 200 days after such report's submission for all other aliens.</p> <p>A victim (or an immediate family member thereof) of a crime committed by any alien who has been issued a final order of removal shall have standing in federal district court to enforce entry and financial prohibitions. </p> <p>The Immigration and Nationality Act is amended to: (1) discontinue granting visas to a subject, national, or resident of a listed country unless the country has issued the appropriate travel documents pursuant to this bill; and (2) grant standing to enforce such provision in federal district court to a victim (or an immediate family member thereof) of a crime committed by any alien who has been issued a final order of removal.</p>]]></text>
          <versionCode>00</versionCode>
          <actionDesc>Introduced in House</actionDesc>
          <name>Introduced in House</name>
        </item>
      </billSummaries>
    </summaries>
    <committeeReports />
    <congress>115</congress>
    <billNumber>82</billNumber>
    <calendarNumbers />
    <amendments />
    <introducedDate>2017-01-03</introducedDate>
    <updateDate>2017-06-19T20:30:32Z</updateDate>
    <subjects>
      <billSubjects>
        <policyArea>
          <name>International Affairs</name>
        </policyArea>
        <legislativeSubjects>
          <item>
            <name>Border security and unlawful immigration</name>
          </item>
          <item>
            <name>Congressional oversight</name>
          </item>
          <item>
            <name>Crime victims</name>
          </item>
          <item>
            <name>Criminal investigation, prosecution, interrogation</name>
          </item>
          <item>
            <name>Foreign aid and international relief</name>
          </item>
          <item>
            <name>Immigration status and procedures</name>
          </item>
          <item>
            <name>Jurisdiction and venue</name>
          </item>
          <item>
            <name>Visas and passports</name>
          </item>
        </legislativeSubjects>
      </billSubjects>
    </subjects>
    <billType>HR</billType>
    <relatedBills />
    <notes />
    <originChamber>House</originChamber>
    <cosponsors>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Brat, Dave [R-VA-7]</fullName>
        <identifiers>
          <gpoId />
          <bioguideId>B001290</bioguideId>
          <lisID>2203</lisID>
        </identifiers>
        <lastName>Brat</lastName>
        <firstName>Dave</firstName>
        <state>VA</state>
        <sponsorshipDate>2017-01-09</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>7</district>
        <party>R</party>
        <middleName />
        <bioguideId>B001290</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Biggs, Andy [R-AZ-5]</fullName>
        <identifiers>
          <bioguideId>B001302</bioguideId>
          <lisID>2307</lisID>
          <gpoId />
        </identifiers>
        <lastName>Biggs</lastName>
        <firstName>Andy</firstName>
        <state>AZ</state>
        <sponsorshipDate>2017-01-09</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>5</district>
        <party>R</party>
        <middleName />
        <bioguideId>B001302</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. King, Steve [R-IA-4]</fullName>
        <identifiers>
          <gpoId>7918</gpoId>
          <bioguideId>K000362</bioguideId>
          <lisID>1724</lisID>
        </identifiers>
        <lastName>King</lastName>
        <firstName>Steve</firstName>
        <state>IA</state>
        <sponsorshipDate>2017-01-09</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>4</district>
        <party>R</party>
        <middleName />
        <bioguideId>K000362</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Smith, Lamar [R-TX-21]</fullName>
        <identifiers>
          <gpoId>8177</gpoId>
          <bioguideId>S000583</bioguideId>
          <lisID>1075</lisID>
        </identifiers>
        <lastName>SMITH</lastName>
        <firstName>LAMAR</firstName>
        <state>TX</state>
        <sponsorshipDate>2017-01-09</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>21</district>
        <party>R</party>
        <middleName />
        <bioguideId>S000583</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Gohmert, Louie [R-TX-1]</fullName>
        <identifiers>
          <lisID>1801</lisID>
          <gpoId>8157</gpoId>
          <bioguideId>G000552</bioguideId>
        </identifiers>
        <lastName>Gohmert</lastName>
        <firstName>Louie</firstName>
        <state>TX</state>
        <sponsorshipDate>2017-01-13</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>1</district>
        <party>R</party>
        <middleName />
        <bioguideId>G000552</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Yoho, Ted S. [R-FL-3]</fullName>
        <identifiers>
          <gpoId />
          <bioguideId>Y000065</bioguideId>
          <lisID>2115</lisID>
        </identifiers>
        <lastName>Yoho</lastName>
        <firstName>Ted</firstName>
        <state>FL</state>
        <sponsorshipDate>2017-01-13</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>3</district>
        <party>R</party>
        <middleName>S.</middleName>
        <bioguideId>Y000065</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Abraham, Ralph Lee [R-LA-5]</fullName>
        <identifiers>
          <gpoId />
          <bioguideId>A000374</bioguideId>
          <lisID>2244</lisID>
        </identifiers>
        <lastName>Abraham</lastName>
        <firstName>Ralph</firstName>
        <state>LA</state>
        <sponsorshipDate>2017-01-13</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>5</district>
        <party>R</party>
        <middleName>Lee</middleName>
        <bioguideId>A000374</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Noem, Kristi L. [R-SD-At Large]</fullName>
        <identifiers>
          <gpoId>8147</gpoId>
          <bioguideId>N000184</bioguideId>
          <lisID>2060</lisID>
        </identifiers>
        <lastName>Noem</lastName>
        <firstName>Kristi</firstName>
        <state>SD</state>
        <sponsorshipDate>2017-01-13</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>0</district>
        <party>R</party>
        <middleName>L.</middleName>
        <bioguideId>N000184</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Cuellar, Henry [D-TX-28]</fullName>
        <identifiers>
          <bioguideId>C001063</bioguideId>
          <lisID>1807</lisID>
          <gpoId>8184</gpoId>
        </identifiers>
        <lastName>Cuellar</lastName>
        <firstName>Henry</firstName>
        <state>TX</state>
        <sponsorshipDate>2017-01-13</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>28</district>
        <party>D</party>
        <middleName />
        <bioguideId>C001063</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Brooks, Mo [R-AL-5]</fullName>
        <identifiers>
          <bioguideId>B001274</bioguideId>
          <lisID>1987</lisID>
          <gpoId>7790</gpoId>
        </identifiers>
        <lastName>Brooks</lastName>
        <firstName>Mo</firstName>
        <state>AL</state>
        <sponsorshipDate>2017-01-13</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>5</district>
        <party>R</party>
        <middleName />
        <bioguideId>B001274</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Calvert, Ken [R-CA-42]</fullName>
        <identifiers>
          <gpoId>7849</gpoId>
          <bioguideId>C000059</bioguideId>
          <lisID>165</lisID>
        </identifiers>
        <lastName>CALVERT</lastName>
        <firstName>KEN</firstName>
        <state>CA</state>
        <sponsorshipDate>2017-01-13</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>42</district>
        <party>R</party>
        <middleName />
        <bioguideId>C000059</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Gaetz, Matt [R-FL-1]</fullName>
        <identifiers>
          <gpoId />
          <bioguideId>G000578</bioguideId>
          <lisID>2314</lisID>
        </identifiers>
        <lastName>Gaetz</lastName>
        <firstName>Matt</firstName>
        <state>FL</state>
        <sponsorshipDate>2017-01-13</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>1</district>
        <party>R</party>
        <middleName />
        <bioguideId>G000578</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Grothman, Glenn [R-WI-6]</fullName>
        <identifiers>
          <gpoId />
          <bioguideId>G000576</bioguideId>
          <lisID>2276</lisID>
        </identifiers>
        <lastName>Grothman</lastName>
        <firstName>Glenn</firstName>
        <state>WI</state>
        <sponsorshipDate>2017-01-13</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>6</district>
        <party>R</party>
        <middleName />
        <bioguideId>G000576</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Cheney, Liz [R-WY-At Large]</fullName>
        <identifiers>
          <bioguideId>C001109</bioguideId>
          <lisID>2356</lisID>
          <gpoId />
        </identifiers>
        <lastName>Cheney</lastName>
        <firstName>Liz</firstName>
        <state>WY</state>
        <sponsorshipDate>2017-01-13</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>0</district>
        <party>R</party>
        <middleName />
        <bioguideId>C001109</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Barr, Andy [R-KY-6]</fullName>
        <identifiers>
          <bioguideId>B001282</bioguideId>
          <lisID>2131</lisID>
          <gpoId />
        </identifiers>
        <lastName>Barr</lastName>
        <firstName>Andy</firstName>
        <state>KY</state>
        <sponsorshipDate>2017-01-13</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>6</district>
        <party>R</party>
        <middleName />
        <bioguideId>B001282</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Hunter, Duncan D. [R-CA-50]</fullName>
        <identifiers>
          <lisID>1909</lisID>
          <gpoId>7857</gpoId>
          <bioguideId>H001048</bioguideId>
        </identifiers>
        <lastName>Hunter</lastName>
        <firstName>Duncan</firstName>
        <state>CA</state>
        <sponsorshipDate>2017-01-13</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>50</district>
        <party>R</party>
        <middleName>D.</middleName>
        <bioguideId>H001048</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Garrett, Thomas A., Jr. [R-VA-5]</fullName>
        <identifiers>
          <gpoId />
          <bioguideId>G000580</bioguideId>
          <lisID>2353</lisID>
        </identifiers>
        <lastName>Garrett</lastName>
        <firstName>Thomas</firstName>
        <state>VA</state>
        <sponsorshipDate>2017-01-13</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>5</district>
        <party>R</party>
        <middleName>A.</middleName>
        <bioguideId>G000580</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Mullin, Markwayne [R-OK-2]</fullName>
        <identifiers>
          <gpoId />
          <bioguideId>M001190</bioguideId>
          <lisID>2156</lisID>
        </identifiers>
        <lastName>Mullin</lastName>
        <firstName>Markwayne</firstName>
        <state>OK</state>
        <sponsorshipDate>2017-01-24</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>2</district>
        <party>R</party>
        <middleName />
        <bioguideId>M001190</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Young, Don [R-AK-At Large]</fullName>
        <identifiers>
          <bioguideId>Y000033</bioguideId>
          <lisID>1256</lisID>
          <gpoId>7785</gpoId>
        </identifiers>
        <lastName>YOUNG</lastName>
        <firstName>DON</firstName>
        <state>AK</state>
        <sponsorshipDate>2017-01-27</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>0</district>
        <party>R</party>
        <middleName>E.</middleName>
        <bioguideId>Y000033</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Rouzer, David [R-NC-7]</fullName>
        <identifiers>
          <gpoId />
          <bioguideId>R000603</bioguideId>
          <lisID>2256</lisID>
        </identifiers>
        <lastName>Rouzer</lastName>
        <firstName>David</firstName>
        <state>NC</state>
        <sponsorshipDate>2017-01-27</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>7</district>
        <party>R</party>
        <middleName />
        <bioguideId>R000603</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Rooney, Francis [R-FL-19]</fullName>
        <identifiers>
          <gpoId />
          <bioguideId>R000607</bioguideId>
          <lisID>2323</lisID>
        </identifiers>
        <lastName>Rooney</lastName>
        <firstName>Francis</firstName>
        <state>FL</state>
        <sponsorshipDate>2017-01-27</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>19</district>
        <party>R</party>
        <middleName />
        <bioguideId>R000607</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Jones, Walter B., Jr. [R-NC-3]</fullName>
        <identifiers>
          <gpoId>8026</gpoId>
          <bioguideId>J000255</bioguideId>
          <lisID>612</lisID>
        </identifiers>
        <lastName>JONES</lastName>
        <firstName>WALTER</firstName>
        <state>NC</state>
        <sponsorshipDate>2017-01-27</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>3</district>
        <party>R</party>
        <middleName>B.</middleName>
        <bioguideId>J000255</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Knight, Stephen [R-CA-25]</fullName>
        <identifiers>
          <gpoId />
          <lisID>2228</lisID>
          <bioguideId>K000387</bioguideId>
        </identifiers>
        <lastName>Knight</lastName>
        <firstName>Stephen</firstName>
        <state>CA</state>
        <sponsorshipDate>2017-01-27</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>25</district>
        <party>R</party>
        <middleName />
        <bioguideId>K000387</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Weber, Randy K., Sr. [R-TX-14]</fullName>
        <identifiers>
          <bioguideId>W000814</bioguideId>
          <lisID>2161</lisID>
          <gpoId />
        </identifiers>
        <lastName>Weber</lastName>
        <firstName>Randy</firstName>
        <state>TX</state>
        <sponsorshipDate>2017-01-27</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>14</district>
        <party>R</party>
        <middleName>K.</middleName>
        <bioguideId>W000814</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Bost, Mike [R-IL-12]</fullName>
        <identifiers>
          <bioguideId>B001295</bioguideId>
          <lisID>2243</lisID>
          <gpoId />
        </identifiers>
        <lastName>Bost</lastName>
        <firstName>Mike</firstName>
        <state>IL</state>
        <sponsorshipDate>2017-01-27</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>12</district>
        <party>R</party>
        <middleName />
        <bioguideId>B001295</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Aderholt, Robert B. [R-AL-4]</fullName>
        <identifiers>
          <bioguideId>A000055</bioguideId>
          <lisID>1460</lisID>
          <gpoId>7789</gpoId>
        </identifiers>
        <lastName>ADERHOLT</lastName>
        <firstName>ROBERT</firstName>
        <state>AL</state>
        <sponsorshipDate>2017-01-27</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>4</district>
        <party>R</party>
        <middleName>B.</middleName>
        <bioguideId>A000055</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Higgins, Clay [R-LA-3]</fullName>
        <identifiers>
          <gpoId />
          <bioguideId>H001077</bioguideId>
          <lisID>2329</lisID>
        </identifiers>
        <lastName>Higgins</lastName>
        <firstName>Clay</firstName>
        <state>LA</state>
        <sponsorshipDate>2017-01-27</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>3</district>
        <party>R</party>
        <middleName />
        <bioguideId>H001077</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Budd, Ted [R-NC-13]</fullName>
        <identifiers>
          <bioguideId>B001305</bioguideId>
          <lisID>2336</lisID>
          <gpoId />
        </identifiers>
        <lastName>Budd</lastName>
        <firstName>Ted</firstName>
        <state>NC</state>
        <sponsorshipDate>2017-01-31</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>13</district>
        <party>R</party>
        <middleName />
        <bioguideId>B001305</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Marchant, Kenny [R-TX-24]</fullName>
        <identifiers>
          <bioguideId>M001158</bioguideId>
          <lisID>1806</lisID>
          <gpoId>8766</gpoId>
        </identifiers>
        <lastName>Marchant</lastName>
        <firstName>Kenny</firstName>
        <state>TX</state>
        <sponsorshipDate>2017-01-31</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>24</district>
        <party>R</party>
        <middleName />
        <bioguideId>M001158</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Johnson, Sam [R-TX-3]</fullName>
        <identifiers>
          <gpoId>8159</gpoId>
          <bioguideId>J000174</bioguideId>
          <lisID>603</lisID>
        </identifiers>
        <lastName>JOHNSON</lastName>
        <firstName>SAM</firstName>
        <state>TX</state>
        <sponsorshipDate>2017-02-01</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>3</district>
        <party>R</party>
        <middleName />
        <bioguideId>J000174</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Young, David [R-IA-3]</fullName>
        <identifiers>
          <gpoId />
          <bioguideId>Y000066</bioguideId>
          <lisID>2242</lisID>
        </identifiers>
        <lastName>Young</lastName>
        <firstName>David</firstName>
        <state>IA</state>
        <sponsorshipDate>2017-02-02</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>3</district>
        <party>R</party>
        <middleName />
        <bioguideId>Y000066</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Meadows, Mark [R-NC-11]</fullName>
        <identifiers>
          <lisID>2142</lisID>
          <gpoId />
          <bioguideId>M001187</bioguideId>
        </identifiers>
        <lastName>Meadows</lastName>
        <firstName>Mark</firstName>
        <state>NC</state>
        <sponsorshipDate>2017-02-03</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>11</district>
        <party>R</party>
        <middleName />
        <bioguideId>M001187</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Fortenberry, Jeff [R-NE-1]</fullName>
        <identifiers>
          <bioguideId>F000449</bioguideId>
          <lisID>1793</lisID>
          <gpoId>8038</gpoId>
        </identifiers>
        <lastName>Fortenberry</lastName>
        <firstName>Jeff</firstName>
        <state>NE</state>
        <sponsorshipDate>2017-02-07</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>1</district>
        <party>R</party>
        <middleName />
        <bioguideId>F000449</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Wilson, Joe [R-SC-2]</fullName>
        <identifiers>
          <gpoId>8142</gpoId>
          <bioguideId>W000795</bioguideId>
          <lisID>1688</lisID>
        </identifiers>
        <lastName>Wilson</lastName>
        <firstName>Joe</firstName>
        <state>SC</state>
        <sponsorshipDate>2017-02-07</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>2</district>
        <party>R</party>
        <middleName />
        <bioguideId>W000795</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Gibbs, Bob [R-OH-7]</fullName>
        <identifiers>
          <lisID>2049</lisID>
          <gpoId>8108</gpoId>
          <bioguideId>G000563</bioguideId>
        </identifiers>
        <lastName>Gibbs</lastName>
        <firstName>Bob</firstName>
        <state>OH</state>
        <sponsorshipDate>2017-02-07</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>7</district>
        <party>R</party>
        <middleName />
        <bioguideId>G000563</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Williams, Roger [R-TX-25]</fullName>
        <identifiers>
          <lisID>2165</lisID>
          <gpoId />
          <bioguideId>W000816</bioguideId>
        </identifiers>
        <lastName>Williams</lastName>
        <firstName>Roger</firstName>
        <state>TX</state>
        <sponsorshipDate>2017-02-07</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>25</district>
        <party>R</party>
        <middleName />
        <bioguideId>W000816</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Wittman, Robert J. [R-VA-1]</fullName>
        <identifiers>
          <bioguideId>W000804</bioguideId>
          <lisID>1886</lisID>
          <gpoId>8192</gpoId>
        </identifiers>
        <lastName>Wittman</lastName>
        <firstName>Robert</firstName>
        <state>VA</state>
        <sponsorshipDate>2017-02-15</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>1</district>
        <party>R</party>
        <middleName>J.</middleName>
        <bioguideId>W000804</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Buck, Ken [R-CO-4]</fullName>
        <identifiers>
          <gpoId />
          <bioguideId>B001297</bioguideId>
          <lisID>2233</lisID>
        </identifiers>
        <lastName>Buck</lastName>
        <firstName>Ken</firstName>
        <state>CO</state>
        <sponsorshipDate>2017-02-21</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>4</district>
        <party>R</party>
        <middleName />
        <bioguideId>B001297</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. LaHood, Darin [R-IL-18]</fullName>
        <identifiers>
          <gpoId />
          <bioguideId>L000585</bioguideId>
          <lisID>2295</lisID>
        </identifiers>
        <lastName>LaHood</lastName>
        <firstName>Darin</firstName>
        <state>IL</state>
        <sponsorshipDate>2017-02-24</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>18</district>
        <party>R</party>
        <middleName />
        <bioguideId>L000585</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Barletta, Lou [R-PA-11]</fullName>
        <identifiers>
          <bioguideId>B001269</bioguideId>
          <lisID>2054</lisID>
          <gpoId>8129</gpoId>
        </identifiers>
        <lastName>Barletta</lastName>
        <firstName>Lou</firstName>
        <state>PA</state>
        <sponsorshipDate>2017-03-01</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>11</district>
        <party>R</party>
        <middleName />
        <bioguideId>B001269</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Burgess, Michael C. [R-TX-26]</fullName>
        <identifiers>
          <bioguideId>B001248</bioguideId>
          <lisID>1751</lisID>
          <gpoId>8182</gpoId>
        </identifiers>
        <lastName>Burgess</lastName>
        <firstName>Michael</firstName>
        <state>TX</state>
        <sponsorshipDate>2017-03-07</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>26</district>
        <party>R</party>
        <middleName>C.</middleName>
        <bioguideId>B001248</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Farenthold, Blake [R-TX-27]</fullName>
        <identifiers>
          <gpoId>8183</gpoId>
          <bioguideId>F000460</bioguideId>
          <lisID>2067</lisID>
        </identifiers>
        <lastName>Farenthold</lastName>
        <firstName>Blake</firstName>
        <state>TX</state>
        <sponsorshipDate>2017-03-08</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>27</district>
        <party>R</party>
        <middleName />
        <bioguideId>F000460</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Banks, Jim [R-IN-3]</fullName>
        <identifiers>
          <bioguideId>B001299</bioguideId>
          <lisID>2326</lisID>
          <gpoId />
        </identifiers>
        <lastName>Banks</lastName>
        <firstName>Jim</firstName>
        <state>IN</state>
        <sponsorshipDate>2017-03-13</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>3</district>
        <party>R</party>
        <middleName />
        <bioguideId>B001299</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Arrington, Jodey C. [R-TX-19]</fullName>
        <identifiers>
          <gpoId />
          <bioguideId>A000375</bioguideId>
          <lisID>2350</lisID>
        </identifiers>
        <lastName>Arrington</lastName>
        <firstName>Jodey</firstName>
        <state>TX</state>
        <sponsorshipDate>2017-03-24</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>19</district>
        <party>R</party>
        <middleName>C.</middleName>
        <bioguideId>A000375</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. McKinley, David B. [R-WV-1]</fullName>
        <identifiers>
          <bioguideId>M001180</bioguideId>
          <lisID>2074</lisID>
          <gpoId>8222</gpoId>
        </identifiers>
        <lastName>McKinley</lastName>
        <firstName>David</firstName>
        <state>WV</state>
        <sponsorshipDate>2017-03-24</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>1</district>
        <party>R</party>
        <middleName>B.</middleName>
        <bioguideId>M001180</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Mooney, Alexander X. [R-WV-2]</fullName>
        <identifiers>
          <bioguideId>M001195</bioguideId>
          <lisID>2277</lisID>
          <gpoId />
        </identifiers>
        <lastName>Mooney</lastName>
        <firstName>Alexander</firstName>
        <state>WV</state>
        <sponsorshipDate>2017-03-27</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>2</district>
        <party>R</party>
        <middleName>X.</middleName>
        <bioguideId>M001195</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. LaMalfa, Doug [R-CA-1]</fullName>
        <identifiers>
          <lisID>2100</lisID>
          <gpoId />
          <bioguideId>L000578</bioguideId>
        </identifiers>
        <lastName>LaMalfa</lastName>
        <firstName>Doug</firstName>
        <state>CA</state>
        <sponsorshipDate>2017-04-08</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>1</district>
        <party>R</party>
        <middleName />
        <bioguideId>L000578</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Duncan, Jeff [R-SC-3]</fullName>
        <identifiers>
          <bioguideId>D000615</bioguideId>
          <lisID>2057</lisID>
          <gpoId>8143</gpoId>
        </identifiers>
        <lastName>Duncan</lastName>
        <firstName>Jeff</firstName>
        <state>SC</state>
        <sponsorshipDate>2017-04-08</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>3</district>
        <party>R</party>
        <middleName />
        <bioguideId>D000615</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Perry, Scott [R-PA-4]</fullName>
        <identifiers>
          <bioguideId>P000605</bioguideId>
          <lisID>2157</lisID>
          <gpoId />
        </identifiers>
        <lastName>Perry</lastName>
        <firstName>Scott</firstName>
        <state>PA</state>
        <sponsorshipDate>2017-04-08</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>4</district>
        <party>R</party>
        <middleName />
        <bioguideId>P000605</bioguideId>
      </item>
      <item>
        <isOriginalCosponsor>False</isOriginalCosponsor>
        <fullName>Rep. Hice, Jody B. [R-GA-10]</fullName>
        <identifiers>
          <bioguideId>H001071</bioguideId>
          <lisID>2237</lisID>
          <gpoId />
        </identifiers>
        <lastName>Hice</lastName>
        <firstName>Jody</firstName>
        <state>GA</state>
        <sponsorshipDate>2017-04-26</sponsorshipDate>
        <sponsorshipWithdrawnDate />
        <district>10</district>
        <party>R</party>
        <middleName>B.</middleName>
        <bioguideId>H001071</bioguideId>
      </item>
    </cosponsors>
    <recordedVotes />
    <titles>
      <item>
        <parentTitleType />
        <titleType>(Extracted from GPO) Short Titles as Introduced</titleType>
        <title>Criminal Alien Deportation Enforcement Act of 2017</title>
        <chamberCode />
        <chamberName />
      </item>
      <item>
        <parentTitleType />
        <titleType>Short Titles as Introduced</titleType>
        <title>Criminal Alien Deportation Enforcement Act of 2017</title>
        <chamberCode />
        <chamberName />
      </item>
      <item>
        <parentTitleType />
        <titleType>Official Title as Introduced</titleType>
        <title>To withhold Federal financial assistance from each country that denies or unreasonably delays the acceptance of nationals of such country who have been ordered removed from the United States and to prohibit the issuance of visas to nationals of such country.</title>
        <chamberCode />
        <chamberName />
      </item>
      <item>
        <parentTitleType />
        <titleType>Display Title</titleType>
        <title>Criminal Alien Deportation Enforcement Act of 2017</title>
        <chamberCode />
        <chamberName />
      </item>
    </titles>
    <version>1.0.0</version>
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
