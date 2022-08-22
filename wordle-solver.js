// Note that the NYT only uses around 2300 possible 5-letter words as correct answers (whereas there are over 12,000 5-letter words in English). So we use the NYT word list, since we're only looking for correct answers!

// Total number of Wordle words used in analysis: 2309
// FREQUENCY TABLE
//
//Letter  Total   |       Pos 1   Pos 2   Pos 3   Pos 4   Pos 5
//--------------------------------------------------------------
//  a      975    |        140     304     306     162     63
//  b      280    |        173     16      56      24      11
//  c      475    |        198     40      56      150     31
//  d      393    |        111     20      75      69      118
//  e      1230   |        72      241     177     318     422
//  f      229    |        135     8       25      35      26
//  g      310    |        115     11      67      76      41
//  h      387    |        69      144     9       28      137
//  i      670    |        34      201     266     158     11
//  j      27     |        20      2       3       2       0
//  k      210    |        20      10      12      55      113
//  l      716    |        87      200     112     162     155
//  m      316    |        107     38      61      68      42
//  n      573    |        37      87      137     182     130
//  o      753    |        41      279     243     132     58
//  p      365    |        141     61      57      50      56
//  q      29     |        23      5       1       0       0
//  r      897    |        105     267     163     150     212
//  s      668    |        365     16      80      171     36
//  t      729    |        149     77      111     139     253
//  u      466    |        33      185     165     82      1
//  v      152    |        43      15      49      45      0
//  w      194    |        82      44      26      25      17
//  x      37     |        0       14      12      3       8
//  y      424    |        6       22      29      3       364
//  z      40     |        3       2       11      20      4

/*
 * Logging
 */
LOGGING_ON = false;

/*
 * Constants
 */
const NUM_LETTERS = 5;
const MAX_ATTEMPTS = 6;
const ROW_DELAY = 5000; // TBD: This seems to work in my setup if browser window remains in focus, but is a cludge. Does NOT work if switch context to another application from the browser.
const TILE_TBD = "tbd";
const TILE_EMPTY = "empty";
const TILE_ABSENT = "absent";
const TILE_PRESENT = "present";
const TILE_CORRECT = "correct";
const BEST_GUESS_THRESHOLD = 50; // TBD: refine as needed
const MOST_COMMON = ["[abcpst]","[aeiloru]","[aeinoru]","[aceilnrs]","[ehlnrty]"];

/* 
 * NYT word list in entirety (2309 words)
 */
const ORIGINAL_WORDS = ["aback","abase","abate","abbey","abbot","abhor","abide","abled","abode","abort","about","above","abuse","abyss","acorn","acrid","actor","acute","adage","adapt","adept","admin","admit","adobe","adopt","adore","adorn","adult","affix","afire","afoot","afoul","after","again","agape","agate","agent","agile","aging","aglow","agony","agree","ahead","aider","aisle","alarm","album","alert","algae","alibi","alien","align","alike","alive","allay","alley","allot","allow","alloy","aloft","alone","along","aloof","aloud","alpha","altar","alter","amass","amaze","amber","amble","amend","amiss","amity","among","ample","amply","amuse","angel","anger","angle","angry","angst","anime","ankle","annex","annoy","annul","anode","antic","anvil","aorta","apart","aphid","aping","apnea","apple","apply","apron","aptly","arbor","ardor","arena","argue","arise","armor","aroma","arose","array","arrow","arson","artsy","ascot","ashen","aside","askew","assay","asset","atoll","atone","attic","audio","audit","augur","aunty","avail","avert","avian","avoid","await","awake","award","aware","awash","awful","awoke","axial","axiom","axion","azure","bacon","badge","badly","bagel","baggy","baker","baler","balmy","banal","banjo","barge","baron","basal","basic","basil","basin","basis","baste","batch","bathe","baton","batty","bawdy","bayou","beach","beady","beard","beast","beech","beefy","befit","began","begat","beget","begin","begun","being","belch","belie","belle","belly","below","bench","beret","berry","berth","beset","betel","bevel","bezel","bible","bicep","biddy","bigot","bilge","billy","binge","bingo","biome","birch","birth","bison","bitty","black","blade","blame","bland","blank","blare","blast","blaze","bleak","bleat","bleed","bleep","blend","bless","blimp","blind","blink","bliss","blitz","bloat","block","bloke","blond","blood","bloom","blown","bluer","bluff","blunt","blurb","blurt","blush","board","boast","bobby","boney","bongo","bonus","booby","boost","booth","booty","booze","boozy","borax","borne","bosom","bossy","botch","bough","boule","bound","bowel","boxer","brace","braid","brain","brake","brand","brash","brass","brave","bravo","brawl","brawn","bread","break","breed","briar","bribe","brick","bride","brief","brine","bring","brink","briny","brisk","broad","broil","broke","brood","brook","broom","broth","brown","brunt","brush","brute","buddy","budge","buggy","bugle","build","built","bulge","bulky","bully","bunch","bunny","burly","burnt","burst","bused","bushy","butch","butte","buxom","buyer","bylaw","cabal","cabby","cabin","cable","cacao","cache","cacti","caddy","cadet","cagey","cairn","camel","cameo","canal","candy","canny","canoe","canon","caper","caput","carat","cargo","carol","carry","carve","caste","catch","cater","catty","caulk","cause","cavil","cease","cedar","cello","chafe","chaff","chain","chair","chalk","champ","chant","chaos","chard","charm","chart","chase","chasm","cheap","cheat","check","cheek","cheer","chess","chest","chick","chide","chief","child","chili","chill","chime","china","chirp","chock","choir","choke","chord","chore","chose","chuck","chump","chunk","churn","chute","cider","cigar","cinch","circa","civic","civil","clack","claim","clamp","clang","clank","clash","clasp","class","clean","clear","cleat","cleft","clerk","click","cliff","climb","cling","clink","cloak","clock","clone","close","cloth","cloud","clout","clove","clown","cluck","clued","clump","clung","coach","coast","cobra","cocoa","colon","color","comet","comfy","comic","comma","conch","condo","conic","copse","coral","corer","corny","couch","cough","could","count","coupe","court","coven","cover","covet","covey","cower","coyly","crack","craft","cramp","crane","crank","crash","crass","crate","crave","crawl","craze","crazy","creak","cream","credo","creed","creek","creep","creme","crepe","crept","cress","crest","crick","cried","crier","crime","crimp","crisp","croak","crock","crone","crony","crook","cross","croup","crowd","crown","crude","cruel","crumb","crump","crush","crust","crypt","cubic","cumin","curio","curly","curry","curse","curve","curvy","cutie","cyber","cycle","cynic","daddy","daily","dairy","daisy","dally","dance","dandy","datum","daunt","dealt","death","debar","debit","debug","debut","decal","decay","decor","decoy","decry","defer","deign","deity","delay","delta","delve","demon","demur","denim","dense","depot","depth","derby","deter","detox","deuce","devil","diary","dicey","digit","dilly","dimly","diner","dingo","dingy","diode","dirge","dirty","disco","ditch","ditto","ditty","diver","dizzy","dodge","dodgy","dogma","doing","dolly","donor","donut","dopey","doubt","dough","dowdy","dowel","downy","dowry","dozen","draft","drain","drake","drama","drank","drape","drawl","drawn","dread","dream","dress","dried","drier","drift","drill","drink","drive","droit","droll","drone","drool","droop","dross","drove","drown","druid","drunk","dryer","dryly","duchy","dully","dummy","dumpy","dunce","dusky","dusty","dutch","duvet","dwarf","dwell","dwelt","dying","eager","eagle","early","earth","easel","eaten","eater","ebony","eclat","edict","edify","eerie","egret","eight","eject","eking","elate","elbow","elder","elect","elegy","elfin","elide","elite","elope","elude","email","embed","ember","emcee","empty","enact","endow","enema","enemy","enjoy","ennui","ensue","enter","entry","envoy","epoch","epoxy","equal","equip","erase","erect","erode","error","erupt","essay","ester","ether","ethic","ethos","etude","evade","event","every","evict","evoke","exact","exalt","excel","exert","exile","exist","expel","extol","extra","exult","eying","fable","facet","faint","fairy","faith","false","fancy","fanny","farce","fatal","fatty","fault","fauna","favor","feast","fecal","feign","fella","felon","femme","femur","fence","feral","ferry","fetal","fetch","fetid","fetus","fever","fewer","fiber","ficus","field","fiend","fiery","fifth","fifty","fight","filer","filet","filly","filmy","filth","final","finch","finer","first","fishy","fixer","fizzy","fjord","flack","flail","flair","flake","flaky","flame","flank","flare","flash","flask","fleck","fleet","flesh","flick","flier","fling","flint","flirt","float","flock","flood","floor","flora","floss","flour","flout","flown","fluff","fluid","fluke","flume","flung","flunk","flush","flute","flyer","foamy","focal","focus","foggy","foist","folio","folly","foray","force","forge","forgo","forte","forth","forty","forum","found","foyer","frail","frame","frank","fraud","freak","freed","freer","fresh","friar","fried","frill","frisk","fritz","frock","frond","front","frost","froth","frown","froze","fruit","fudge","fugue","fully","fungi","funky","funny","furor","furry","fussy","fuzzy","gaffe","gaily","gamer","gamma","gamut","gassy","gaudy","gauge","gaunt","gauze","gavel","gawky","gayer","gayly","gazer","gecko","geeky","geese","genie","genre","ghost","ghoul","giant","giddy","gipsy","girly","girth","given","giver","glade","gland","glare","glass","glaze","gleam","glean","glide","glint","gloat","globe","gloom","glory","gloss","glove","glyph","gnash","gnome","godly","going","golem","golly","gonad","goner","goody","gooey","goofy","goose","gorge","gouge","gourd","grace","grade","graft","grail","grain","grand","grant","grape","graph","grasp","grass","grate","grave","gravy","graze","great","greed","green","greet","grief","grill","grime","grimy","grind","gripe","groan","groin","groom","grope","gross","group","grout","grove","growl","grown","gruel","gruff","grunt","guard","guava","guess","guest","guide","guild","guile","guilt","guise","gulch","gully","gumbo","gummy","guppy","gusto","gusty","gypsy","habit","hairy","halve","handy","happy","hardy","harem","harpy","harry","harsh","haste","hasty","hatch","hater","haunt","haute","haven","havoc","hazel","heady","heard","heart","heath","heave","heavy","hedge","hefty","heist","helix","hello","hence","heron","hilly","hinge","hippo","hippy","hitch","hoard","hobby","hoist","holly","homer","honey","honor","horde","horny","horse","hotel","hotly","hound","house","hovel","hover","howdy","human","humid","humor","humph","humus","hunch","hunky","hurry","husky","hussy","hutch","hydro","hyena","hymen","hyper","icily","icing","ideal","idiom","idiot","idler","idyll","igloo","iliac","image","imbue","impel","imply","inane","inbox","incur","index","inept","inert","infer","ingot","inlay","inlet","inner","input","inter","intro","ionic","irate","irony","islet","issue","itchy","ivory","jaunt","jazzy","jelly","jerky","jetty","jewel","jiffy","joint","joist","joker","jolly","joust","judge","juice","juicy","jumbo","jumpy","junta","junto","juror","kappa","karma","kayak","kebab","khaki","kinky","kiosk","kitty","knack","knave","knead","kneed","kneel","knelt","knife","knock","knoll","known","koala","krill","label","labor","laden","ladle","lager","lance","lanky","lapel","lapse","large","larva","lasso","latch","later","lathe","latte","laugh","layer","leach","leafy","leaky","leant","leapt","learn","lease","leash","least","leave","ledge","leech","leery","lefty","legal","leggy","lemon","lemur","leper","level","lever","libel","liege","light","liken","lilac","limbo","limit","linen","liner","lingo","lipid","lithe","liver","livid","llama","loamy","loath","lobby","local","locus","lodge","lofty","logic","login","loopy","loose","lorry","loser","louse","lousy","lover","lower","lowly","loyal","lucid","lucky","lumen","lumpy","lunar","lunch","lunge","lupus","lurch","lurid","lusty","lying","lymph","lyric","macaw","macho","macro","madam","madly","mafia","magic","magma","maize","major","maker","mambo","mamma","mammy","manga","mange","mango","mangy","mania","manic","manly","manor","maple","march","marry","marsh","mason","masse","match","matey","mauve","maxim","maybe","mayor","mealy","meant","meaty","mecca","medal","media","medic","melee","melon","mercy","merge","merit","merry","metal","meter","metro","micro","midge","midst","might","milky","mimic","mince","miner","minim","minor","minty","minus","mirth","miser","missy","mocha","modal","model","modem","mogul","moist","molar","moldy","money","month","moody","moose","moral","moron","morph","mossy","motel","motif","motor","motto","moult","mound","mount","mourn","mouse","mouth","mover","movie","mower","mucky","mucus","muddy","mulch","mummy","munch","mural","murky","mushy","music","musky","musty","myrrh","nadir","naive","nanny","nasal","nasty","natal","naval","navel","needy","neigh","nerdy","nerve","never","newer","newly","nicer","niche","niece","night","ninja","ninny","ninth","noble","nobly","noise","noisy","nomad","noose","north","nosey","notch","novel","nudge","nurse","nutty","nylon","nymph","oaken","obese","occur","ocean","octal","octet","odder","oddly","offal","offer","often","olden","older","olive","ombre","omega","onion","onset","opera","opine","opium","optic","orbit","order","organ","other","otter","ought","ounce","outdo","outer","outgo","ovary","ovate","overt","ovine","ovoid","owing","owner","oxide","ozone","paddy","pagan","paint","paler","palsy","panel","panic","pansy","papal","paper","parer","parka","parry","parse","party","pasta","paste","pasty","patch","patio","patsy","patty","pause","payee","payer","peace","peach","pearl","pecan","pedal","penal","pence","penne","penny","perch","peril","perky","pesky","pesto","petal","petty","phase","phone","phony","photo","piano","picky","piece","piety","piggy","pilot","pinch","piney","pinky","pinto","piper","pique","pitch","pithy","pivot","pixel","pixie","pizza","place","plaid","plain","plait","plane","plank","plant","plate","plaza","plead","pleat","plied","plier","pluck","plumb","plume","plump","plunk","plush","poesy","point","poise","poker","polar","polka","polyp","pooch","poppy","porch","poser","posit","posse","pouch","pound","pouty","power","prank","prawn","preen","press","price","prick","pride","pried","prime","primo","print","prior","prism","privy","prize","probe","prone","prong","proof","prose","proud","prove","prowl","proxy","prude","prune","psalm","pubic","pudgy","puffy","pulpy","pulse","punch","pupil","puppy","puree","purer","purge","purse","pushy","putty","pygmy","quack","quail","quake","qualm","quark","quart","quash","quasi","queen","queer","quell","query","quest","queue","quick","quiet","quill","quilt","quirk","quite","quota","quote","quoth","rabbi","rabid","racer","radar","radii","radio","rainy","raise","rajah","rally","ralph","ramen","ranch","randy","range","rapid","rarer","raspy","ratio","ratty","raven","rayon","razor","reach","react","ready","realm","rearm","rebar","rebel","rebus","rebut","recap","recur","recut","reedy","refer","refit","regal","rehab","reign","relax","relay","relic","remit","renal","renew","repay","repel","reply","rerun","reset","resin","retch","retro","retry","reuse","revel","revue","rhino","rhyme","rider","ridge","rifle","right","rigid","rigor","rinse","ripen","riper","risen","riser","risky","rival","river","rivet","roach","roast","robin","robot","rocky","rodeo","roger","rogue","roomy","roost","rotor","rouge","rough","round","rouse","route","rover","rowdy","rower","royal","ruddy","ruder","rugby","ruler","rumba","rumor","rupee","rural","rusty","sadly","safer","saint","salad","sally","salon","salsa","salty","salve","salvo","sandy","saner","sappy","sassy","satin","satyr","sauce","saucy","sauna","saute","savor","savoy","savvy","scald","scale","scalp","scaly","scamp","scant","scare","scarf","scary","scene","scent","scion","scoff","scold","scone","scoop","scope","score","scorn","scour","scout","scowl","scram","scrap","scree","screw","scrub","scrum","scuba","sedan","seedy","segue","seize","semen","sense","sepia","serif","serum","serve","setup","seven","sever","sewer","shack","shade","shady","shaft","shake","shaky","shale","shall","shalt","shame","shank","shape","shard","share","shark","sharp","shave","shawl","shear","sheen","sheep","sheer","sheet","sheik","shelf","shell","shied","shift","shine","shiny","shire","shirk","shirt","shoal","shock","shone","shook","shoot","shore","shorn","short","shout","shove","shown","showy","shrew","shrub","shrug","shuck","shunt","shush","shyly","siege","sieve","sight","sigma","silky","silly","since","sinew","singe","siren","sissy","sixth","sixty","skate","skier","skiff","skill","skimp","skirt","skulk","skull","skunk","slack","slain","slang","slant","slash","slate","sleek","sleep","sleet","slept","slice","slick","slide","slime","slimy","sling","slink","sloop","slope","slosh","sloth","slump","slung","slunk","slurp","slush","slyly","smack","small","smart","smash","smear","smell","smelt","smile","smirk","smite","smith","smock","smoke","smoky","smote","snack","snail","snake","snaky","snare","snarl","sneak","sneer","snide","sniff","snipe","snoop","snore","snort","snout","snowy","snuck","snuff","soapy","sober","soggy","solar","solid","solve","sonar","sonic","sooth","sooty","sorry","sound","south","sower","space","spade","spank","spare","spark","spasm","spawn","speak","spear","speck","speed","spell","spelt","spend","spent","sperm","spice","spicy","spied","spiel","spike","spiky","spill","spilt","spine","spiny","spire","spite","splat","split","spoil","spoke","spoof","spook","spool","spoon","spore","sport","spout","spray","spree","sprig","spunk","spurn","spurt","squad","squat","squib","stack","staff","stage","staid","stain","stair","stake","stale","stalk","stall","stamp","stand","stank","stare","stark","start","stash","state","stave","stead","steak","steal","steam","steed","steel","steep","steer","stein","stern","stick","stiff","still","stilt","sting","stink","stint","stock","stoic","stoke","stole","stomp","stone","stony","stood","stool","stoop","store","stork","storm","story","stout","stove","strap","straw","stray","strip","strut","stuck","study","stuff","stump","stung","stunk","stunt","style","suave","sugar","suing","suite","sulky","sully","sumac","sunny","super","surer","surge","surly","sushi","swami","swamp","swarm","swash","swath","swear","sweat","sweep","sweet","swell","swept","swift","swill","swine","swing","swirl","swish","swoon","swoop","sword","swore","sworn","swung","synod","syrup","tabby","table","taboo","tacit","tacky","taffy","taint","taken","taker","tally","talon","tamer","tango","tangy","taper","tapir","tardy","tarot","taste","tasty","tatty","taunt","tawny","teach","teary","tease","teddy","teeth","tempo","tenet","tenor","tense","tenth","tepee","tepid","terra","terse","testy","thank","theft","their","theme","there","these","theta","thick","thief","thigh","thing","think","third","thong","thorn","those","three","threw","throb","throw","thrum","thumb","thump","thyme","tiara","tibia","tidal","tiger","tight","tilde","timer","timid","tipsy","titan","tithe","title","toast","today","toddy","token","tonal","tonga","tonic","tooth","topaz","topic","torch","torso","torus","total","totem","touch","tough","towel","tower","toxic","toxin","trace","track","tract","trade","trail","train","trait","tramp","trash","trawl","tread","treat","trend","triad","trial","tribe","trice","trick","tried","tripe","trite","troll","troop","trope","trout","trove","truce","truck","truer","truly","trump","trunk","truss","trust","truth","tryst","tubal","tuber","tulip","tulle","tumor","tunic","turbo","tutor","twang","tweak","tweed","tweet","twice","twine","twirl","twist","twixt","tying","udder","ulcer","ultra","umbra","uncle","uncut","under","undid","undue","unfed","unfit","unify","union","unite","unity","unlit","unmet","unset","untie","until","unwed","unzip","upper","upset","urban","urine","usage","usher","using","usual","usurp","utile","utter","vague","valet","valid","valor","value","valve","vapid","vapor","vault","vaunt","vegan","venom","venue","verge","verse","verso","verve","vicar","video","vigil","vigor","villa","vinyl","viola","viper","viral","virus","visit","visor","vista","vital","vivid","vixen","vocal","vodka","vogue","voice","voila","vomit","voter","vouch","vowel","vying","wacky","wafer","wager","wagon","waist","waive","waltz","warty","waste","watch","water","waver","waxen","weary","weave","wedge","weedy","weigh","weird","welch","welsh","whack","whale","wharf","wheat","wheel","whelp","where","which","whiff","while","whine","whiny","whirl","whisk","white","whole","whoop","whose","widen","wider","widow","width","wield","wight","willy","wimpy","wince","winch","windy","wiser","wispy","witch","witty","woken","woman","women","woody","wooer","wooly","woozy","wordy","world","worry","worse","worst","worth","would","wound","woven","wrack","wrath","wreak","wreck","wrest","wring","wrist","write","wrong","wrote","wrung","wryly","yacht","yearn","yeast","yield","young","youth","zebra","zesty","zonal"]

/*
 * Used words as of this creation
 * TBD: leverage an external file instead so it can reflect addition of each day's answer. 
 * Won't be able to inject jQuery parseJSON() to read an external text file
 * because of CORS -- NYT prevents cross-origin, where an external website's contents cannot be used in NYT JS.
 * Therefore, perhaps would need to use some other method.
 * Note also that I don't currently have a good way to automatically update an external text file, so this would be moot until then anyway.
 */
const USED_WORDS = ["waste","treat","shrug","twang","twice","gruel","poker","khaki","hunky","label","glean","cling","patty","unfit","smear","alien","buggy","rhyme","youth","coyly","quart","cramp","bluff","upset","stomp","motto","cinch","elope","power","midge","tryst","aphid","trite","angry","flock","wacky","roomy","wedge","liver","bland","night","madam","berth","stead","voice","agape","fluff","field","sever","lilac","egret","pinto","hutch","gawky","droll","retro","rusty","beady","smite","brink","awful","gloat","input","loser","cacao","blown","apron","primo","atone","donor","float","goose","piety","girth","trait","flood","gloom","depth","froth","phase","showy","creak","manor","atoll","bayou","crept","tiara","asset","vouch","album","hinge","money","scrap","gamer","glass","scour","being","delve","yield","metal","tipsy","slung","farce","gecko","shine","canny","midst","badge","homer","train","story","hairy","forgo","larva","trash","zesty","shown","heist","askew","inert","olive","plant","oxide","cargo","foyer","flair","ample","cheek","shame","mince","chunk","royal","squad","black","stair","scare","foray","comma","natal","shawl","fewer","trope","snout","lowly","stove","shall","found","nymph","epoxy","depot","chest","purge","slosh","their","renew","allow","saute","movie","cater","tease","smelt","focus","today","watch","lapse","month","sweet","hoard","cloth","brine","ahead","mourn","nasty","rupee","choke","chant","spill","vivid","bloke","trove","thorn","other","tacit","swill","dodge","shake","caulk","aroma","cynic","robin","ultra","ulcer","pause","humor","frame","elder","skill","aloft","pleat","shard","moist","those","light","wrung","could","perky","mount","whack","sugar","knoll","crimp","wince","prick","robot","point","proxy","shire","solar","panic","tangy","abbey","favor","drink","query","gorge","crank","slump","banal","tiger","siege","truss","boost","rebus","unify","troll","tapir","aside","ferry","acute","picky","weary","gripe","craze","pluck","brake","baton","champ","peach","using","trace","vital","sonic","masse","conic","viral","rhino","break","triad","epoch","usher","exult","grime","cheat","solve","bring","prove","store","tilde","clock","wrote","retch","perch","rouge","radio","surer","finer","vodka","heron","chill","gaudy","pithy","smart","badly","rogue","group","fixer","groin","duchy","coast","blurt","pulpy","altar","great","briar","click","gouge","world","erode","boozy","dozen","fling","growl","abyss","steed","enema","jaunt","comet","tweed","pilot","dutch","belch","ought","dowry","thumb","hyper","hatch","alone","motor","aback","guild","kebab","spend","fjord","essay","spray","spicy","agate","salad","basic","moult","corny","forge","civic","islet","labor","gamma","lying","audit","round","loopy","lusty","golem","goner","greet","start","lapel","biome","parry","shrub","front","wooer","totem","flick","delta","bleed","argue","swirl","error","agree","offal","flume","crass","panel","stout","bribe","drain","yearn","print","seedy","ivory","belly","stand","first","forth","booby","flesh","unmet","linen","maxim","pound","mimic","spike","cluck","crate","digit","repay","sower","crazy","adobe","outdo","trawl","whelp","unfed","paper","staff","croak","helix","floss","pride","batty","react","marry","abase","colon","stool","crust","fresh","death","major","feign","abate","bench","quiet","grade","stink","karma","model","dwarf","heath","serve","naval","evade","focal","blush","awake","humph","sissy","rebut","cigar"];

/*
 * Helper functions -- BEGIN
 */
const delay = ms => new Promise(res => setTimeout(res, ms));

function logger(text) {
    if (LOGGING_ON) {
        console.log(text);
    }
}

function determineNextGuess(result) {
    const greens = ['.', '.', '.', '.', '.'];
    const yellows = ['.', '.', '.', '.', '.'];
    const blacks = ['', '', '', '', ''];

    let regex = "";
    let yellowLetters = "";
    
    // GREEN HANDLING
    let greenfound = false;
    for (let x=0; x < NUM_LETTERS; x++) {
        if (result[x] === TILE_CORRECT) {
            greenfound = true;
            greens[x] = guess[x];
        }
    }
    if (greenfound) {
        regex = new RegExp(greens.join(""));
        possibleWords.forEach((value) => {
            if (value.search(regex) == -1) { // Word doesn't match, so remove
                possibleWords.delete(value);
            }
        });
        logger("Greens:");
        logger(greens);
        logger(possibleWords);
    }

    // YELLOW HANDLING -- pass 1
    let yellowFound = false;
    for (let x=0; x < NUM_LETTERS; x++) {
        if (result[x] === TILE_PRESENT) {
            yellowFound = true;
            yellows[x] = "[^" + guess[x] + "]";
            yellowLetters += guess[x];
        }
    }
    if (yellowFound) { 
        regex = new RegExp(yellows.join(""));
        possibleWords.forEach((value) => {
            if (value.search(regex) == -1) { // Word doesn't match, so remove
                possibleWords.delete(value);
            }
        });
        logger("Yellows:");
        logger(yellows);
        logger(possibleWords);
    }

    // BLACK HANDLING
    let blackFound = false;
    for (let x=0; x < NUM_LETTERS; x++) {
        if (result[x] === TILE_ABSENT) {
            blackFound = true;
            for (let y=0; y < NUM_LETTERS; y++) {
                if ((greens.join("").search(guess[x]) == -1)  && (yellows.join("").search(guess[x]) == -1)) {
                    // ensure that this letter truly does not exist elsewhere, since TILE_ABSENT can be for a letter that is present somewhere, but there's not another one (e.g., you've entered FLUFF, but the word is BLUFF, the first "F" will be TILE_ABSENT)
                    blacks[y] = blacks[y] + guess[x];
                }
            }
        }
    }
    if (blackFound) {
        for (let z=0; z < NUM_LETTERS; z++) {
            if (blacks[z] != '') {
                blacks[z] = "[^" + blacks[z] + "]";
            }
            else {
                blacks[z] = '.';
            }
        }
        regex = new RegExp(blacks.join(""));
        possibleWords.forEach((value) => {
            if (value.search(regex) == -1) { // Word doesn't match, so remove
                possibleWords.delete(value);
            }
        });
        logger("Blacks:");
        logger(blacks);
        logger(possibleWords);
    }

    // YELLOW HANDLING -- pass 2
    if (yellowFound) {
        for (i=0; i < yellowLetters.length; i++) {
            possibleWords.forEach((value) => {
                if (value.search(yellowLetters[i]) == -1) { // Word doesn't match, so remove
                    possibleWords.delete(value);
                }
            });
        }
        logger("Final words with letters " + yellowLetters + ":");
        logger(possibleWords);
    }

    if (possibleWords.size > BEST_GUESS_THRESHOLD) {
        return makeBestGuess(greens.join(""));
    }
    else {
        let finalWords = []; // Need to use array to be able to randomly select a word using an index
        possibleWords.forEach((value) => {
            finalWords.push(value);
        });
        logger("Final words: " + finalWords);
        return finalWords[Math.floor(Math.random() * finalWords.length)];
    }
}

function makeBestGuess(greenLetters) {
    let regex = "";
    let matchString = "";
    let guessWords = new Set(possibleWords); // Don't want to mess with the possibleWords, since this is a best guess to narrow down further
    if (greenLetters === ".....") { // This section is not flexible for a non-5-letter Wordle, but who cares since we're only handling one solution herein
        regex = new RegExp(MOST_COMMON[0] + MOST_COMMON[1] + MOST_COMMON[2] + MOST_COMMON[3] + MOST_COMMON[4]);
    }
    else {
        for (let x=0; x < NUM_LETTERS; x++) {
            if (greenLetters[x].search('.') != -1) {
                matchString += MOST_COMMON[x];
            }
            else {
                matchString += "[" + greenLetters[x] + "]";
            }
        }
        regex = new RegExp(matchString);
    }
    logger("Best guess filters:");
    logger(regex);
    guessWords.forEach((value) => {
        if (value.search(regex) == -1) { // Word doesn't match, so remove
            guessWords.delete(value);
        }
    });
    let bestGuessWords = []; // Need to use an array to be able to randomly select a word
    guessWords.forEach((value) => {
        bestGuessWords.push(value);
    });
    logger("Best guess words:");
    logger(bestGuessWords);
    let randomGuess = bestGuessWords[Math.floor(Math.random() * bestGuessWords.length)];

    // Toss best guesses with repeated letters
    let repeatedLetter = false;
    for (let x=0; x < NUM_LETTERS; x++) {
        if (repeatedLetter) {
            break;
        }
        for (let y=x+1; y < NUM_LETTERS; y++) {
            if (randomGuess[x].search(randomGuess[y]) != -1) {
                repeatedLetter = true;
                logger("Repeated letter in " + randomGuess + ", so toss");
                break;
            }
        }
    }
    if (!repeatedLetter) {
        return randomGuess;
    }
    else {
        return makeBestGuess(greenLetters); // recursive as needed to try again to find a good word
    }
}

const submitGuess = async (word) => {
    for (const letter of word) {
        window.dispatchEvent(new KeyboardEvent('keydown', {
            'key': letter
        }));
        await delay(Math.floor(Math.random() * 500) + 100); // randomly-timed delay between keypresses (100-500 ms)
    }
    window.dispatchEvent(new KeyboardEvent('keydown', {
        'key': "Enter"
    }));
}

function solved(rowNum) {
    let correct = false;
    if (rowNum != 1) { // First time through the row is empty since haven't guessed yet, so cannot assess
        const tiles = wordleRows[rowNum - 2].getElementsByClassName("Tile-module_tile__3ayIZ");
        let dataState = null;
        correct = true;
        let pos = 0;
        for (const tile of tiles) {
            dataState = tile.getAttribute('data-state');
            currResult[pos] = dataState;
            pos += 1; 
            if (dataState != TILE_CORRECT) {
                correct = false;
            }
        }
        infoText.innerText += "\nRow " + (rowNum-1) + ": " + currResult;
    }
    return correct;
}
/*
 * Helper functions -- END
 */

/*
 * Webpage UI -- BEGIN
 */
const overlay = document.getElementsByClassName("Modal-module_modalOverlay__81ZCi");
const boardStart = document.getElementsByClassName("Board-module_boardContainer__cKb-C");
const solveButton = document.createElement('button');
const infoText = document.createElement('div');
const wordleRows = document.getElementsByClassName("Row-module_row__dEHfN");

if (overlay[0] != null) {
    overlay[0].remove();
}
solveButton.setAttribute('id', "solve");
solveButton.innerText = "Click to solve";
solveButton.addEventListener('click', () => {
    solveWordle();
});
infoText.innerText = "";
boardStart[0].insertAdjacentElement('beforebegin', solveButton);
solveButton.insertAdjacentElement('afterend', infoText);
/*
 * Webpage UI -- END
 */

/*
 * Global tracking variables -- BEGIN
 */
let guess = null;
let solutionFound = false;
let currResult = [TILE_EMPTY, TILE_EMPTY, TILE_EMPTY, TILE_EMPTY, TILE_EMPTY];
const possibleWords = new Set();
for (const origWord of ORIGINAL_WORDS) {
    if (USED_WORDS.includes(origWord) === false) {
        possibleWords.add(origWord);
    }
}
/*
 * Global tracking variables -- END
 */

/*
 * Main solver -- BEGIN
 */
const solveWordle = async() => {
    let row = 1;
    infoText.innerText = "Attempting to solve...";
    while (!solutionFound && row <= MAX_ATTEMPTS) {
        if (!solved(row)) {
            logger("\n=== NEXT ROW ===");
            guess = determineNextGuess(currResult); 
            if (!guess) {
                // Something went wrong. Shouldn't happen, but exit gracefully
                infoText.innerText += "\nSomething went wrong, ran out of words, so exiting.";
                logger("Something went wrong, ran out of words, so exiting.");
                break;
            }
            else{
                infoText.innerText += "\nGuess " + row + ": \'" + guess + "\'" + " out of " + possibleWords.size + " possible word(s)";
                submitGuess(guess);
                possibleWords.delete(guess); // this isn't necessary, I suppose
                await delay(ROW_DELAY); // Have to wait for the letters to be entered and for the delays of rendering
                row += 1;
            }
        }
        else {
            solutionFound = true;
            infoText.innerText += "\nSOLVED!";
        }
    }
}
/*
 * Main solver -- END
 */
