%% ProMatch persisted data (auto-generated).
%% Do not edit manually unless you know what you are doing.

next_meeting_id(55).
event(e1, 'AI & Startups Matchmaking', '2025-12-20', 'Online').
event(e2, 'Fintech & Banking B2B', '2026-01-15', 'Skopje').
event(e3, 'My matchmaking event', '2025-12-15', 'Tetovo').
participant(irfa, 'Irfan', 'irfan@seeu.edu.mk', 'SEEU').
participant(p1, 'Alice', 'alice@startupa.com', 'Startup A').
participant(p10, 'Judy', 'judy@accelerator.org', 'Startup Accelerator').
participant(p2, 'Bob', 'bob@investorfund.com', 'Investor Fund').
participant(p3, 'Carol', 'carol@bigcorp.com', 'BigCorp').
participant(p4, 'Dave', 'dave@ai-lab.org', 'AI Lab').
participant(p5, 'Eli', 'eli@devhouse.io', 'DevHouse').
participant(p6, 'Fatima', 'fatima@city-hospital.org', 'City Hospital').
role(irfa, student).
role(p1, startup).
role(p10, mentor).
role(p2, investor).
role(p3, corporate).
role(p4, research).
role(p5, engineer).
role(p6, doctor).
participant_event(irfa, e3).
participant_event(p1, e1).
participant_event(p10, e1).
participant_event(p2, e1).
participant_event(p3, e1).
participant_event(p3, e2).
participant_event(p4, e1).
participant_event(p4, e2).
participant_event(p5, e1).
participant_event(p6, e1).
profile(p1, [ai, startups, saas], [product, early_stage], [investment, mentoring]).
profile(p10, [startups, acceleration, ai], [mentoring, network], [strong_teams, high_potential_startups]).
profile(p2, [ai, venture_capital], [investment, mentoring], [dealflow, innovative_startups]).
profile(p3, [fintech, banking], [partnerships, corporate_resources], [innovation, pilots]).
profile(p4, [ai, ml_research], [research, algorithms], [industry_partners, datasets]).
profile(p5, [ai, devtools, platforms], [engineering_support, prototypes], [interesting_startups, b2b_products]).
profile(p6, [healthtech, ai, clinical_workflows], [clinical_expertise], [better_tools, research_collaboration]).
timeslot(e1, s1, '10:00 - 10:20').
timeslot(e1, s2, '10:25 - 10:45').
timeslot(e1, s3, '11:00 - 11:20').
timeslot(e2, s1, '14:00 - 14:20').
timeslot(e2, s2, '14:25 - 14:45').
timeslot(e2, s4, '11:30-13:40').
timeslot(e3, s1, '11:30-12:12').
meeting(2, e2, p3, p4, s1, manual).
meeting(52, e1, p1, p10, s1, auto).
meeting(53, e1, p2, p3, s1, auto).
meeting(54, e1, p4, p5, s1, auto).
