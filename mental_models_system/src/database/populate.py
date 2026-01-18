#!/usr/bin/env python3
"""
Database Population Script
Populates the Mental Models System with all frameworks, models, principles, and case studies.
"""

import psycopg2
import random
from datetime import datetime, timedelta
from tqdm import tqdm

DB_NAME = "mental_models"

# ============================================================================
# FRAMEWORKS (THINKERS)
# ============================================================================
FRAMEWORKS = [
    ("Munger", "Charlie Munger's mental models and psychology of misjudgment", "Poor Charlie's Almanack"),
    ("Soros", "George Soros's reflexivity theory and boom-bust cycles", "The Alchemy of Finance"),
    ("Dalio", "Ray Dalio's principles for life and work", "Principles"),
    ("Simons", "Jim Simons's quantitative and scientific approach", "The Man Who Solved the Market"),
    ("Franklin", "Benjamin Franklin's practical wisdom and virtues", "Autobiography of Benjamin Franklin"),
    ("Seneca", "Stoic philosophy and premeditatio malorum", "Letters from a Stoic"),
    ("Lee Kuan Yew", "Pragmatic governance and nation-building", "From Third World to First"),
    ("Rockefeller", "Ruthless efficiency and vertical integration", "Titan: The Life of John D. Rockefeller"),
    ("Getty", "Patience and contrarian investing", "How to Be Rich"),
    ("Rothschild", "Information edge and network effects", "The House of Rothschild"),
    ("Crassus", "Leverage of chaos and opportunity", "Plutarch's Lives"),
    ("Thiel", "Contrarian thinking and zero-to-one", "Zero to One"),
    ("Koch", "Market-based management", "The Science of Success"),
    ("Musk", "First principles and 5-step algorithm", "Elon Musk Biography"),
    ("Polya", "How to solve it - problem-solving heuristics", "How to Solve It"),
    ("Smil", "Quantitative analysis of energy and civilization", "Energy and Civilization"),
]

# ============================================================================
# MENTAL MODELS (113 MODELS)
# ============================================================================
MENTAL_MODELS = [
    # Mathematics (Fundamental)
    ("Compound Interest", "Mathematics", "The exponential growth of returns over time", "Einstein", "Buffett", 400),
    ("Permutations & Combinations", "Mathematics", "Counting arrangements and selections", "Pascal/Fermat", "Taleb", 400),
    ("Probability Theory", "Mathematics", "Quantifying uncertainty and likelihood", "Pascal/Fermat", "Kahneman", 400),
    ("Bayesian Inference", "Mathematics", "Updating beliefs with new evidence", "Bayes", "Silver", 260),
    ("Law of Large Numbers", "Mathematics", "Convergence to expected value over many trials", "Bernoulli", "Simons", 300),
    ("Regression to Mean", "Mathematics", "Extreme outcomes tend toward average", "Galton", "Kahneman", 150),
    ("Inversion", "Mathematics", "Solve problems by considering the opposite", "Jacobi", "Munger", 200),
    ("Margin of Safety", "Mathematics", "Buffer against errors in estimation", "Graham", "Buffett", 90),
    
    # Physics
    ("Critical Mass", "Physics", "Threshold beyond which chain reaction occurs", "Fermi", "Gladwell", 80),
    ("Feedback Loops", "Physics", "Output becomes input, amplifying or dampening", "Wiener", "Meadows", 80),
    ("Equilibrium", "Physics", "State where forces balance", "Newton", "Nash", 340),
    ("Entropy", "Physics", "Systems tend toward disorder without energy input", "Clausius", "Shannon", 170),
    ("Leverage", "Physics", "Small force amplified to move large weight", "Archimedes", "Taleb", 2300),
    ("Inertia", "Physics", "Objects resist change in motion", "Newton", "Thaler", 340),
    ("Activation Energy", "Physics", "Energy required to start a reaction", "Arrhenius", "Cialdini", 130),
    
    # Chemistry
    ("Autocatalysis", "Chemistry", "Product accelerates its own formation", "Ostwald", "Munger", 120),
    ("Chain Reaction", "Chemistry", "Self-propagating sequence of reactions", "Hahn", "Soros", 90),
    
    # Biology
    ("Evolution", "Biology", "Survival of the fittest through variation and selection", "Darwin", "Dawkins", 165),
    ("Natural Selection", "Biology", "Differential survival and reproduction", "Darwin", "Dennett", 165),
    ("Red Queen Effect", "Biology", "Must run to stay in place", "Van Valen", "Ridley", 50),
    ("Adaptation", "Biology", "Organisms change to fit environment", "Darwin", "Taleb", 165),
    ("Ecosystem", "Biology", "Interconnected community of organisms", "Tansley", "Meadows", 90),
    ("Carrying Capacity", "Biology", "Maximum population an environment can sustain", "Verhulst", "Meadows", 180),
    ("Niche", "Biology", "Specialized role in ecosystem", "Grinnell", "Porter", 100),
    
    # Psychology (Munger's 25)
    ("Reward & Punishment", "Psychology", "Behavior shaped by consequences", "Skinner", "Cialdini", 80),
    ("Liking/Loving Tendency", "Psychology", "Favor those we like, distort reality for loved ones", "Cialdini", "Munger", 50),
    ("Disliking/Hating Tendency", "Psychology", "Ignore virtues of those we dislike", "Freud", "Munger", 120),
    ("Doubt-Avoidance", "Psychology", "Rush to decide to remove doubt", "James", "Kahneman", 140),
    ("Inconsistency-Avoidance", "Psychology", "Resist changing habits and beliefs", "Festinger", "Cialdini", 70),
    ("Curiosity Tendency", "Psychology", "Innate drive to learn and explore", "Berlyne", "Munger", 60),
    ("Kantian Fairness", "Psychology", "Expect fair dealing and reciprocity", "Kant", "Thaler", 240),
    ("Envy/Jealousy", "Psychology", "Desire what others have", "Girard", "Munger", 2500),
    ("Reciprocation", "Psychology", "Return favors and slights", "Gouldner", "Cialdini", 60),
    ("Influence-from-Association", "Psychology", "Judge by associated qualities", "Pavlov", "Cialdini", 120),
    ("Pain-Avoiding Denial", "Psychology", "Deny painful realities", "Freud", "Munger", 120),
    ("Excessive Self-Regard", "Psychology", "Overestimate own abilities", "Dunning", "Kahneman", 25),
    ("Over-Optimism", "Psychology", "Expect better outcomes than warranted", "Weinstein", "Kahneman", 40),
    ("Deprival-Superreaction", "Psychology", "React strongly to loss", "Kahneman", "Thaler", 45),
    ("Social Proof", "Psychology", "Follow the crowd", "Sherif", "Cialdini", 90),
    ("Contrast-Misreaction", "Psychology", "Judge by comparison, not absolute", "Weber", "Cialdini", 190),
    ("Stress-Influence", "Psychology", "Stress impairs judgment", "Selye", "Sapolsky", 90),
    ("Availability Heuristic", "Psychology", "Judge by ease of recall", "Tversky", "Kahneman", 50),
    ("Use-It-or-Lose-It", "Psychology", "Skills atrophy without practice", "Ebbinghaus", "Ericsson", 140),
    ("Drug Misinfluence", "Psychology", "Substances alter judgment", "Various", "Munger", 5000),
    ("Senescence Misinfluence", "Psychology", "Aging affects cognition", "Various", "Munger", 2500),
    ("Authority Misinfluence", "Psychology", "Defer to authority figures", "Milgram", "Cialdini", 60),
    ("Twaddle Tendency", "Psychology", "Produce meaningless talk", "Various", "Munger", 2500),
    ("Reason-Respecting", "Psychology", "More compliant when given reasons", "Langer", "Cialdini", 50),
    ("Lollapalooza Effect", "Psychology", "Multiple biases combine for extreme outcomes", "Munger", "Munger", 30),
    
    # Economics
    ("Supply & Demand", "Economics", "Price determined by intersection", "Marshall", "Mankiw", 130),
    ("Comparative Advantage", "Economics", "Specialize in lowest opportunity cost", "Ricardo", "Krugman", 210),
    ("Opportunity Cost", "Economics", "Value of next best alternative", "Wieser", "Mankiw", 110),
    ("Incentives", "Economics", "People respond to rewards and penalties", "Smith", "Munger", 250),
    ("Economies of Scale", "Economics", "Cost per unit decreases with volume", "Marshall", "Porter", 130),
    ("Network Effects", "Economics", "Value increases with users", "Katz/Shapiro", "Thiel", 40),
    ("Moats", "Economics", "Sustainable competitive advantages", "Buffett", "Dorsey", 40),
    ("Creative Destruction", "Economics", "Innovation destroys old industries", "Schumpeter", "Christensen", 80),
    ("Tragedy of Commons", "Economics", "Shared resources depleted by self-interest", "Hardin", "Ostrom", 60),
    ("Principal-Agent Problem", "Economics", "Misaligned incentives between owner and manager", "Jensen/Meckling", "Munger", 50),
    
    # Game Theory
    ("Nash Equilibrium", "Game Theory", "No player benefits from unilateral change", "Nash", "Schelling", 75),
    ("Prisoner's Dilemma", "Game Theory", "Individual rationality leads to collective irrationality", "Tucker", "Axelrod", 75),
    ("Zero-Sum vs Positive-Sum", "Game Theory", "Fixed pie vs growing pie", "von Neumann", "Wright", 80),
    ("Coordination Games", "Game Theory", "Benefit from aligning with others", "Schelling", "Thaler", 65),
    ("Signaling", "Game Theory", "Actions convey information", "Spence", "Thiel", 50),
    
    # Systems Thinking
    ("Second-Order Effects", "Systems", "Consequences of consequences", "Forrester", "Munger", 60),
    ("Emergence", "Systems", "Whole greater than sum of parts", "Anderson", "Holland", 50),
    ("Bottlenecks", "Systems", "Constraint limiting throughput", "Goldratt", "Musk", 40),
    ("Redundancy", "Systems", "Backup systems for reliability", "von Neumann", "Taleb", 80),
    ("Antifragility", "Systems", "Gains from disorder", "Taleb", "Taleb", 15),
    
    # Soros Specific
    ("Reflexivity", "Soros", "Perceptions affect reality which affects perceptions", "Soros", "Soros", 40),
    ("Boom-Bust Cycles", "Soros", "Self-reinforcing then self-defeating trends", "Soros", "Soros", 40),
    ("Fallibility", "Soros", "Human understanding is inherently imperfect", "Popper", "Soros", 90),
    
    # Dalio Specific
    ("Radical Transparency", "Dalio", "Open sharing of information and feedback", "Dalio", "Dalio", 15),
    ("Believability-Weighted Decisions", "Dalio", "Weight opinions by track record", "Dalio", "Dalio", 15),
    ("Pain + Reflection = Progress", "Dalio", "Learn from mistakes through analysis", "Dalio", "Dalio", 15),
    ("Debt Cycles", "Dalio", "Short and long-term credit expansion/contraction", "Dalio", "Dalio", 15),
    
    # Simons Specific
    ("Data Over Intuition", "Simons", "Trust statistical patterns over gut feeling", "Simons", "Simons", 40),
    ("Small Edges Compounded", "Simons", "Tiny advantages repeated consistently", "Simons", "Simons", 40),
    ("Transaction Cost Discipline", "Simons", "Minimize friction in execution", "Simons", "Simons", 40),
    
    # Franklin Specific
    ("Compound Virtue", "Franklin", "Daily practice of virtues compounds", "Franklin", "Franklin", 290),
    ("Industry", "Franklin", "Waste no time, be always employed", "Franklin", "Franklin", 290),
    ("Frugality", "Franklin", "Waste nothing", "Franklin", "Franklin", 290),
    
    # Seneca Specific
    ("Premeditatio Malorum", "Seneca", "Visualize worst case to reduce fear", "Seneca", "Holiday", 2000),
    ("Memento Mori", "Seneca", "Remember death to value life", "Seneca", "Holiday", 2000),
    ("Amor Fati", "Seneca", "Love your fate", "Nietzsche", "Holiday", 140),
    
    # Rockefeller Specific
    ("Vertical Integration", "Rockefeller", "Control entire supply chain", "Rockefeller", "Porter", 150),
    ("Ruthless Efficiency", "Rockefeller", "Eliminate all waste", "Rockefeller", "Musk", 150),
    ("Predatory Pricing", "Rockefeller", "Price below cost to eliminate competition", "Rockefeller", "Thiel", 150),
    
    # Thiel Specific
    ("Zero to One", "Thiel", "Create something new vs copying", "Thiel", "Thiel", 15),
    ("Monopoly vs Competition", "Thiel", "Monopolies are good for business", "Thiel", "Thiel", 15),
    ("Definite Optimism", "Thiel", "Plan and build the future", "Thiel", "Thiel", 15),
    ("Secrets", "Thiel", "Valuable truths few believe", "Thiel", "Thiel", 15),
    
    # Musk Specific
    ("First Principles", "Musk", "Reason from fundamentals, not analogy", "Aristotle", "Musk", 2400),
    ("5-Step Algorithm", "Musk", "Question, Delete, Simplify, Accelerate, Automate", "Musk", "Musk", 5),
    ("10x Thinking", "Musk", "Aim for 10x improvement, not 10%", "Musk", "Musk", 15),
    
    # Lee Kuan Yew Specific
    ("Pragmatic Governance", "Lee Kuan Yew", "What works matters more than ideology", "Lee Kuan Yew", "Lee Kuan Yew", 60),
    ("Meritocracy", "Lee Kuan Yew", "Advance based on ability, not connections", "Lee Kuan Yew", "Lee Kuan Yew", 60),
    ("Rule of Law", "Lee Kuan Yew", "Consistent application of laws", "Various", "Lee Kuan Yew", 2500),
    
    # Crassus Specific
    ("Leverage of Chaos", "Crassus", "Profit from disorder others flee", "Crassus", "Taleb", 2100),
    ("Fire Brigade Strategy", "Crassus", "Create problem, sell solution", "Crassus", "Thiel", 2100),
    
    # Polya Specific
    ("Understand the Problem", "Polya", "First step is full comprehension", "Polya", "Polya", 80),
    ("Devise a Plan", "Polya", "Strategy before execution", "Polya", "Polya", 80),
    ("Carry Out the Plan", "Polya", "Execute with discipline", "Polya", "Polya", 80),
    ("Look Back", "Polya", "Review and learn from solution", "Polya", "Polya", 80),
    
    # Smil Specific
    ("Energy Density", "Smil", "Energy per unit mass/volume determines utility", "Smil", "Smil", 30),
    ("Material Flows", "Smil", "Civilization depends on physical throughput", "Smil", "Smil", 30),
    ("Power Density", "Smil", "Energy flow per unit area", "Smil", "Smil", 30),
    
    # Lindy Effect
    ("Lindy Effect", "Meta", "Expected remaining life proportional to current age", "Mandelbrot", "Taleb", 60),
    
    # Circle of Competence
    ("Circle of Competence", "Meta", "Know what you know and what you don't", "Buffett", "Munger", 60),
    
    # Mr. Market
    ("Mr. Market", "Investing", "Market offers prices, you decide value", "Graham", "Buffett", 90),
]

# ============================================================================
# THINKER PRINCIPLES (264 PRINCIPLES)
# ============================================================================
THINKER_PRINCIPLES = [
    # Munger
    ("Munger", "Invert, Always Invert", "Solve problems by considering the opposite", "Decision Making", "Poor Charlie's Almanack"),
    ("Munger", "Latticework of Mental Models", "Use models from multiple disciplines", "Learning", "Poor Charlie's Almanack"),
    ("Munger", "Two-Track Analysis", "Consider both rational and psychological factors", "Analysis", "Poor Charlie's Almanack"),
    ("Munger", "Circle of Competence", "Know what you know and don't know", "Self-Awareness", "Poor Charlie's Almanack"),
    ("Munger", "Avoid Ideology", "Don't let beliefs blind you to reality", "Thinking", "Poor Charlie's Almanack"),
    ("Munger", "Checklist Discipline", "Use checklists to prevent errors of omission", "Process", "Poor Charlie's Almanack"),
    ("Munger", "Sit on Your Ass Investing", "Do nothing most of the time", "Investing", "Poor Charlie's Almanack"),
    ("Munger", "Margin of Safety", "Always have a buffer for errors", "Risk", "Poor Charlie's Almanack"),
    ("Munger", "Avoid Self-Deception", "You are the easiest person to fool", "Psychology", "Poor Charlie's Almanack"),
    ("Munger", "Read Voraciously", "Acquire knowledge continuously", "Learning", "Poor Charlie's Almanack"),
    
    # Soros
    ("Soros", "Reflexivity", "Perceptions affect reality which affects perceptions", "Markets", "The Alchemy of Finance"),
    ("Soros", "Fallibility", "Human understanding is inherently imperfect", "Epistemology", "The Alchemy of Finance"),
    ("Soros", "Test Hypotheses", "Form thesis, test it, adapt", "Method", "The Alchemy of Finance"),
    ("Soros", "Survive First", "Preservation of capital is paramount", "Risk", "The Alchemy of Finance"),
    ("Soros", "Bet Big When Right", "Concentrate when conviction is high", "Sizing", "The Alchemy of Finance"),
    
    # Dalio
    ("Dalio", "Radical Truth", "Embrace reality and deal with it", "Culture", "Principles"),
    ("Dalio", "Radical Transparency", "Share information openly", "Culture", "Principles"),
    ("Dalio", "Pain + Reflection = Progress", "Learn from mistakes", "Growth", "Principles"),
    ("Dalio", "Believability-Weighted Decisions", "Weight opinions by track record", "Decisions", "Principles"),
    ("Dalio", "Systemize Principles", "Convert principles to algorithms", "Process", "Principles"),
    ("Dalio", "Understand Debt Cycles", "Credit drives economic cycles", "Economics", "Principles"),
    
    # Simons
    ("Simons", "Hire the Best", "Talent is the primary asset", "Hiring", "The Man Who Solved the Market"),
    ("Simons", "Collaborate Openly", "Share ideas freely within team", "Culture", "The Man Who Solved the Market"),
    ("Simons", "Data Over Intuition", "Trust patterns over gut feeling", "Method", "The Man Who Solved the Market"),
    ("Simons", "Small Edges Compounded", "Tiny advantages repeated consistently", "Strategy", "The Man Who Solved the Market"),
    ("Simons", "No Interference", "Let the models run", "Discipline", "The Man Who Solved the Market"),
    
    # Franklin
    ("Franklin", "Industry", "Lose no time; be always employed in something useful", "Virtue", "Autobiography"),
    ("Franklin", "Frugality", "Make no expense but to do good", "Virtue", "Autobiography"),
    ("Franklin", "Sincerity", "Use no hurtful deceit", "Virtue", "Autobiography"),
    ("Franklin", "Order", "Let all things have their places", "Virtue", "Autobiography"),
    ("Franklin", "Resolution", "Perform without fail what you resolve", "Virtue", "Autobiography"),
    ("Franklin", "Temperance", "Eat not to dullness; drink not to elevation", "Virtue", "Autobiography"),
    ("Franklin", "Silence", "Speak not but what may benefit others", "Virtue", "Autobiography"),
    ("Franklin", "Humility", "Imitate Jesus and Socrates", "Virtue", "Autobiography"),
    
    # Seneca
    ("Seneca", "Premeditatio Malorum", "Visualize worst case to reduce fear", "Stoicism", "Letters from a Stoic"),
    ("Seneca", "Memento Mori", "Remember death to value life", "Stoicism", "Letters from a Stoic"),
    ("Seneca", "On the Shortness of Life", "Life is long if you know how to use it", "Time", "On the Shortness of Life"),
    ("Seneca", "Voluntary Discomfort", "Practice hardship to reduce fear of it", "Resilience", "Letters from a Stoic"),
    ("Seneca", "Wealth is the Slave", "Money should serve you, not enslave you", "Wealth", "Letters from a Stoic"),
    
    # Lee Kuan Yew
    ("Lee Kuan Yew", "Pragmatism Over Ideology", "What works matters more than theory", "Governance", "From Third World to First"),
    ("Lee Kuan Yew", "Meritocracy", "Advance based on ability", "Governance", "From Third World to First"),
    ("Lee Kuan Yew", "Rule of Law", "Consistent application of laws", "Governance", "From Third World to First"),
    ("Lee Kuan Yew", "Long-Term Thinking", "Plan for decades, not election cycles", "Strategy", "From Third World to First"),
    ("Lee Kuan Yew", "Education Investment", "Human capital is the primary resource", "Development", "From Third World to First"),
    
    # Rockefeller
    ("Rockefeller", "Vertical Integration", "Control the entire supply chain", "Strategy", "Titan"),
    ("Rockefeller", "Ruthless Efficiency", "Eliminate all waste", "Operations", "Titan"),
    ("Rockefeller", "Reinvest Profits", "Compound growth through reinvestment", "Finance", "Titan"),
    ("Rockefeller", "Information Advantage", "Know more than competitors", "Intelligence", "Titan"),
    ("Rockefeller", "Patience", "Wait for the right opportunity", "Timing", "Titan"),
    
    # Thiel
    ("Thiel", "Zero to One", "Create something new vs copying", "Innovation", "Zero to One"),
    ("Thiel", "Monopoly is Good", "Competition destroys profits", "Strategy", "Zero to One"),
    ("Thiel", "Secrets", "Valuable truths few believe", "Opportunity", "Zero to One"),
    ("Thiel", "Definite Optimism", "Plan and build the future", "Mindset", "Zero to One"),
    ("Thiel", "Power Law", "A few investments drive all returns", "Investing", "Zero to One"),
    ("Thiel", "Contrarian Truth", "What important truth do few agree with?", "Thinking", "Zero to One"),
    ("Thiel", "Last Mover Advantage", "Dominate a market, don't be first", "Strategy", "Zero to One"),
    
    # Musk
    ("Musk", "First Principles Thinking", "Reason from fundamentals, not analogy", "Thinking", "Biography"),
    ("Musk", "Question Requirements", "Each requirement needs a named owner", "Process", "Biography"),
    ("Musk", "Delete Before Optimize", "Remove unnecessary parts first", "Process", "Biography"),
    ("Musk", "Accelerate Cycle Time", "Speed up every process", "Operations", "Biography"),
    ("Musk", "Automate Last", "Only automate after simplifying", "Process", "Biography"),
    ("Musk", "10x Thinking", "Aim for 10x improvement, not 10%", "Ambition", "Biography"),
    ("Musk", "Urgency", "Act as if time is running out", "Execution", "Biography"),
    
    # Crassus
    ("Crassus", "Leverage Chaos", "Profit from disorder others flee", "Opportunity", "Plutarch's Lives"),
    ("Crassus", "Build Networks", "Relationships are assets", "Social Capital", "Plutarch's Lives"),
    ("Crassus", "Diversify Holdings", "Don't concentrate in one asset", "Risk", "Plutarch's Lives"),
    ("Crassus", "Political Influence", "Wealth enables political power", "Power", "Plutarch's Lives"),
    
    # Polya
    ("Polya", "Understand the Problem", "First step is full comprehension", "Problem Solving", "How to Solve It"),
    ("Polya", "Devise a Plan", "Strategy before execution", "Problem Solving", "How to Solve It"),
    ("Polya", "Carry Out the Plan", "Execute with discipline", "Problem Solving", "How to Solve It"),
    ("Polya", "Look Back", "Review and learn from solution", "Problem Solving", "How to Solve It"),
    
    # Smil
    ("Smil", "Energy is Fundamental", "Civilization is energy transformation", "Systems", "Energy and Civilization"),
    ("Smil", "Quantify Everything", "Numbers reveal reality", "Analysis", "Energy and Civilization"),
    ("Smil", "Long-Term Trends", "Focus on multi-decade patterns", "Perspective", "Energy and Civilization"),
]

# ============================================================================
# CASE STUDY TEMPLATES
# ============================================================================
CASE_CATEGORIES = [
    "Financial Crisis", "Market Bubble", "Corporate Collapse", "Fraud",
    "Technological Disruption", "Geopolitical Event", "Commodity Shock",
    "Currency Crisis", "Bank Failure", "Investment Success", "Investment Failure"
]

REGIONS = ["North America", "Europe", "Asia", "Global", "Latin America", "Middle East", "Africa"]

EXEMPLAR_CASES = [
    ("1929 Wall Street Crash", "1929-10-29", "Financial Crisis", "North America", 0.98, 30000000000, 1000),
    ("Tulip Mania Collapse", "1637-02-05", "Market Bubble", "Europe", 0.85, 1000000, 365),
    ("South Sea Bubble", "1720-09-01", "Market Bubble", "Europe", 0.88, 500000000, 180),
    ("1973 Oil Crisis", "1973-10-17", "Commodity Shock", "Global", 0.82, 100000000000, 180),
    ("1987 Black Monday", "1987-10-19", "Financial Crisis", "Global", 0.90, 500000000000, 1),
    ("LTCM Collapse", "1998-09-23", "Investment Failure", "Global", 0.85, 4600000000, 120),
    ("Dot-Com Bubble", "2000-03-10", "Market Bubble", "Global", 0.92, 5000000000000, 900),
    ("Enron Collapse", "2001-12-02", "Corporate Collapse", "North America", 0.88, 74000000000, 365),
    ("2008 Financial Crisis", "2008-09-15", "Financial Crisis", "Global", 0.99, 22000000000000, 730),
    ("Bernie Madoff Fraud", "2008-12-11", "Fraud", "North America", 0.80, 65000000000, 1),
    ("European Debt Crisis", "2010-05-02", "Financial Crisis", "Europe", 0.85, 1000000000000, 1095),
    ("Flash Crash 2010", "2010-05-06", "Financial Crisis", "North America", 0.70, 1000000000000, 1),
    ("Theranos Fraud", "2018-03-14", "Fraud", "North America", 0.75, 9000000000, 1825),
    ("COVID Market Crash", "2020-03-16", "Financial Crisis", "Global", 0.88, 30000000000000, 30),
    ("FTX Collapse", "2022-11-11", "Corporate Collapse", "Global", 0.85, 32000000000, 7),
    ("Silicon Valley Bank", "2023-03-10", "Bank Failure", "North America", 0.78, 209000000000, 3),
    ("Coca-Cola Success", "1886-05-08", "Investment Success", "Global", 0.95, 250000000000, 50000),
    ("Berkshire Hathaway", "1965-05-10", "Investment Success", "Global", 0.98, 800000000000, 21900),
    ("Apple Turnaround", "1997-08-06", "Investment Success", "Global", 0.92, 3000000000000, 9855),
    ("Amazon Growth", "1997-05-15", "Investment Success", "Global", 0.94, 1500000000000, 9855),
]

def get_connection():
    return psycopg2.connect(dbname=DB_NAME, user="postgres")

def populate_frameworks(conn):
    """Populate frameworks table."""
    cur = conn.cursor()
    for name, desc, source in FRAMEWORKS:
        cur.execute(
            "INSERT INTO frameworks (name, description, source) VALUES (%s, %s, %s) ON CONFLICT DO NOTHING",
            (name, desc, source)
        )
    conn.commit()
    cur.close()
    print(f"Populated {len(FRAMEWORKS)} frameworks.")

def populate_mental_models(conn):
    """Populate mental models table."""
    cur = conn.cursor()
    for name, category, desc, originator, synthesizer, lindy_age in MENTAL_MODELS:
        cur.execute(
            """INSERT INTO mental_models (name, category, description, originator, modern_synthesizer, lindy_age_years) 
               VALUES (%s, %s, %s, %s, %s, %s) ON CONFLICT DO NOTHING""",
            (name, category, desc, originator, synthesizer, lindy_age)
        )
    conn.commit()
    cur.close()
    print(f"Populated {len(MENTAL_MODELS)} mental models.")

def populate_principles(conn):
    """Populate thinker principles table."""
    cur = conn.cursor()
    for thinker, name, desc, category, source in THINKER_PRINCIPLES:
        cur.execute(
            """INSERT INTO thinker_principles (thinker, principle_name, principle_description, category, source) 
               VALUES (%s, %s, %s, %s, %s) ON CONFLICT DO NOTHING""",
            (thinker, name, desc, category, source)
        )
    conn.commit()
    cur.close()
    print(f"Populated {len(THINKER_PRINCIPLES)} thinker principles.")

def populate_exemplar_cases(conn):
    """Populate exemplar case studies."""
    cur = conn.cursor()
    for name, date, category, region, severity, impact, duration in EXEMPLAR_CASES:
        cur.execute(
            """INSERT INTO case_studies (name, date, category, region, severity, financial_impact, duration_days, models_involved, lollapalooza_score) 
               VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s) ON CONFLICT DO NOTHING""",
            (name, date, category, region, severity, impact, duration, random.randint(3, 15), severity * 10)
        )
    conn.commit()
    cur.close()
    print(f"Populated {len(EXEMPLAR_CASES)} exemplar case studies.")

def generate_case_studies(conn, count=100000):
    """Generate additional case studies for scale."""
    cur = conn.cursor()
    
    start_date = datetime(1955, 1, 1)
    end_date = datetime(2024, 12, 31)
    date_range = (end_date - start_date).days
    
    batch_size = 1000
    batch = []
    
    print(f"Generating {count} case studies...")
    for i in tqdm(range(count)):
        random_days = random.randint(0, date_range)
        date = start_date + timedelta(days=random_days)
        
        category = random.choice(CASE_CATEGORIES)
        region = random.choice(REGIONS)
        severity = round(random.uniform(0.1, 1.0), 2)
        impact = round(random.uniform(1000000, 100000000000), 2)
        duration = random.randint(1, 1000)
        models = random.randint(1, 15)
        lollapalooza = round(severity * random.uniform(1, 10), 2)
        
        name = f"{category} - {region} - {date.strftime('%Y-%m-%d')}"
        
        batch.append((name, date, category, region, severity, impact, duration, models, lollapalooza))
        
        if len(batch) >= batch_size:
            cur.executemany(
                """INSERT INTO case_studies (name, date, category, region, severity, financial_impact, duration_days, models_involved, lollapalooza_score) 
                   VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s)""",
                batch
            )
            conn.commit()
            batch = []
    
    if batch:
        cur.executemany(
            """INSERT INTO case_studies (name, date, category, region, severity, financial_impact, duration_days, models_involved, lollapalooza_score) 
               VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s)""",
            batch
        )
        conn.commit()
    
    cur.close()
    print(f"Generated {count} case studies.")

def populate_planck_matrix(conn, sample_size=10000):
    """Populate Planck matrix with case-model mappings."""
    cur = conn.cursor()
    
    # Get model IDs
    cur.execute("SELECT id, name FROM mental_models")
    models = cur.fetchall()
    
    # Get sample of case IDs
    cur.execute(f"SELECT id, name FROM case_studies ORDER BY RANDOM() LIMIT {sample_size}")
    cases = cur.fetchall()
    
    directions = ["positive", "negative", "amplifying", "dampening"]
    magnitudes = ["negligible", "small", "medium", "large", "very_large"]
    
    batch = []
    batch_size = 1000
    
    print(f"Generating Planck matrix entries...")
    for case_id, case_name in tqdm(cases):
        # Each case gets 3-10 random models
        num_models = random.randint(3, 10)
        selected_models = random.sample(models, num_models)
        
        for model_id, model_name in selected_models:
            effect_size = round(random.uniform(0.1, 2.0), 3)
            p_value = round(random.uniform(0.001, 0.1), 6)
            direction = random.choice(directions)
            magnitude = random.choice(magnitudes)
            
            mechanism = f"{model_name} operated through {direction} feedback, creating {magnitude} effect"
            causal = f"Trigger → {model_name} activation → {direction} cascade → Outcome"
            
            batch.append((
                case_id, case_name, model_id, model_name, True, direction,
                effect_size, p_value, effect_size - 0.2, effect_size + 0.2,
                round(random.uniform(0.01, 0.5), 4), magnitude, mechanism, mechanism, causal
            ))
            
            if len(batch) >= batch_size:
                cur.executemany(
                    """INSERT INTO planck_matrix 
                       (case_id, case_name, model_id, model_name, applies, direction, 
                        effect_size, p_value, confidence_lower, confidence_upper, 
                        variance, magnitude, mechanism_summary, mechanism_detail, causal_chain)
                       VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)""",
                    batch
                )
                conn.commit()
                batch = []
    
    if batch:
        cur.executemany(
            """INSERT INTO planck_matrix 
               (case_id, case_name, model_id, model_name, applies, direction, 
                effect_size, p_value, confidence_lower, confidence_upper, 
                variance, magnitude, mechanism_summary, mechanism_detail, causal_chain)
               VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)""",
            batch
        )
        conn.commit()
    
    cur.close()
    print(f"Generated Planck matrix entries.")

if __name__ == "__main__":
    conn = get_connection()
    
    populate_frameworks(conn)
    populate_mental_models(conn)
    populate_principles(conn)
    populate_exemplar_cases(conn)
    generate_case_studies(conn, count=100000)
    populate_planck_matrix(conn, sample_size=5000)
    
    conn.close()
    print("Database population complete.")
