:- use_module(library(filesex)).
:- use_module(library(date)).
:- use_module(library(ansi_term)).
:- use_module(library(readutil)).
:- use_module(library(http/json)).
:- use_module(library(process)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(listing)).

% Knowledge base
:- dynamic symptom/1, cause/1, solution/2, symptom_cause/2, confidence/2, contradiction/2, symptom_probability/3.
:- dynamic kb_autosave_file/1, knowledge_autosave_enabled/1, kb_autosave_suspended/1, learned_fact/1, confidence_weight/2, current_locale/1, diagnosis_mode/1, analytics_file/1.

kb_autosave_file('data/kb_dynamic.pl').
analytics_file('data/analytics.jsonl').
voice_script('scripts/voice_capture.py').
knowledge_autosave_enabled(true).
kb_autosave_suspended(false).
current_locale(en).
diagnosis_mode(interactive).

supported_locale(en, 'English').
supported_locale(es, 'Español').

locale_text(en, menu_title, 'Choose an option:').
locale_text(en, menu_option_1, 'Start diagnosis (guided)').
locale_text(en, menu_option_2, 'Start diagnosis (natural language)').
locale_text(en, menu_option_3, 'Start diagnosis (voice input)').
locale_text(en, menu_option_4, 'Learn new symptom').
locale_text(en, menu_option_5, 'Learn new cause').
locale_text(en, menu_option_6, 'Learn new solution').
locale_text(en, menu_option_7, 'View knowledge base').
locale_text(en, menu_option_8, 'Save knowledge to file').
locale_text(en, menu_option_9, 'Load knowledge from file').
locale_text(en, menu_option_10, 'Show analytics summary').
locale_text(en, menu_option_11, 'Change language').
locale_text(en, menu_option_12, 'Exit').
locale_text(en, invalid_choice, 'Invalid choice. Please try again.').
locale_text(en, menu_prompt, 'Select an option (enter a number 1-12): ').
locale_text(en, conflict_choice_prompt, 'Select an option (enter 1-3): ').
locale_text(en, view_kb_prompt, 'Select what to view (enter a number 1-7): ').
locale_text(en, welcome_message, 'Welcome to the Advanced Computer Problem Diagnosis Expert System!').
locale_text(en, input_instruction, 'Answer with yes., no., or unsure.').
locale_text(en, contradiction_notice, 'Detected a contradiction: ~w and ~w cannot both be true.').
locale_text(en, contradiction_resolution_prompt, 'Choose how to resolve the conflict:').
locale_text(en, contradiction_option_1, 'Keep ~w').
locale_text(en, contradiction_option_2, 'Keep ~w').
locale_text(en, contradiction_option_3, 'Mark both as unsure').
locale_text(en, invalid_conflict_choice, 'Invalid choice. Please enter 1, 2, or 3.').
locale_text(en, summary_title, 'Session Summary').
locale_text(en, summary_positive, 'Confirmed symptoms (~d): ~w').
locale_text(en, summary_negative, 'Ruled out symptoms (~d): ~w').
locale_text(en, summary_unsure, 'Uncertain symptoms (~d): ~w').
locale_text(en, summary_none, 'No symptoms recorded in this category.').
locale_text(en, analytics_logged, 'Session analytics appended.').
locale_text(en, analytics_none, 'No analytics recorded yet.').
locale_text(en, language_prompt, 'Enter locale code (e.g., en, es): ').
locale_text(en, language_updated, 'Language updated to ~w.').
locale_text(en, language_invalid, 'Unsupported locale.').
locale_text(en, language_options_header, 'Available languages:').
locale_text(en, natural_language_offer, 'Would you like to describe the issue in your own words first? (yes./no.) ').
locale_text(en, natural_language_prompt, 'Describe the problem (end with a period): ').
locale_text(en, natural_language_none, 'No free-text description captured. Proceeding with guided questions.').
locale_text(en, natural_language_summary, 'Prefilled symptoms from description: ~w').
locale_text(en, voice_unavailable, 'Voice recognition prerequisites not installed. Falling back to guided mode.').
locale_text(en, voice_prompt, 'Speak your issue after the tone. Press Enter when finished.').
locale_text(en, analytics_header, 'Analytics Summary').
locale_text(en, analytics_total_sessions, 'Total sessions: ~d').
locale_text(en, analytics_top_causes, 'Top causes: ~w').
locale_text(en, followup_overheating_loud_fan, 'Detected overheating. Are the fans unusually loud?').
locale_text(en, followup_overheating_dust, 'Detected overheating. Is the machine noticeably dusty inside?').
locale_text(en, followup_slow_performance_disk_full, 'Slow performance observed. Is the disk almost full?').
locale_text(en, followup_wifi_disconnects_router, 'Wi-Fi drops noted. Is the router regularly rebooting or losing signal?').
locale_text(en, followup_frequent_crashes_blue_screen, 'Frequent crashes reported. Do blue screen errors appear?').
locale_text(en, followup_display_artifacts_gpu_heat, 'Visual artifacts detected. Is the GPU temperature running hot?').

locale_text(es, menu_title, 'Seleccione una opción:').
locale_text(es, menu_option_1, 'Iniciar diagnóstico (guiado)').
locale_text(es, menu_option_2, 'Iniciar diagnóstico (lenguaje natural)').
locale_text(es, menu_option_3, 'Iniciar diagnóstico (voz)').
locale_text(es, menu_option_4, 'Aprender nuevo síntoma').
locale_text(es, menu_option_5, 'Aprender nueva causa').
locale_text(es, menu_option_6, 'Aprender nueva solución').
locale_text(es, menu_option_7, 'Ver base de conocimientos').
locale_text(es, menu_option_8, 'Guardar conocimientos en archivo').
locale_text(es, menu_option_9, 'Cargar conocimientos desde archivo').
locale_text(es, menu_option_10, 'Mostrar resumen de analíticas').
locale_text(es, menu_option_11, 'Cambiar idioma').
locale_text(es, menu_option_12, 'Salir').
locale_text(es, invalid_choice, 'Opción inválida. Inténtelo de nuevo.').
locale_text(es, menu_prompt, 'Seleccione una opción (ingrese un número 1-12): ').
locale_text(es, conflict_choice_prompt, 'Seleccione una opción (ingrese un número 1-3): ').
locale_text(es, view_kb_prompt, 'Seleccione qué ver (ingrese un número 1-7): ').
locale_text(es, welcome_message, '¡Bienvenido al Sistema Experto Avanzado de Diagnóstico de Computadoras!').
locale_text(es, input_instruction, 'Responda con yes., no., o unsure.').
locale_text(es, contradiction_notice, 'Se detectó una contradicción: ~w y ~w no pueden ser verdaderos a la vez.').
locale_text(es, contradiction_resolution_prompt, 'Elija cómo resolver el conflicto:').
locale_text(es, contradiction_option_1, 'Mantener ~w').
locale_text(es, contradiction_option_2, 'Mantener ~w').
locale_text(es, contradiction_option_3, 'Marcar ambos como unsure').
locale_text(es, invalid_conflict_choice, 'Opción inválida. Ingrese 1, 2 o 3.').
locale_text(es, summary_title, 'Resumen de la sesión').
locale_text(es, summary_positive, 'Síntomas confirmados (~d): ~w').
locale_text(es, summary_negative, 'Síntomas descartados (~d): ~w').
locale_text(es, summary_unsure, 'Síntomas inciertos (~d): ~w').
locale_text(es, summary_none, 'No se registraron síntomas en esta categoría.').
locale_text(es, analytics_logged, 'Analítica de la sesión guardada.').
locale_text(es, analytics_none, 'Aún no hay analíticas registradas.').
locale_text(es, language_prompt, 'Ingrese el código de idioma (p. ej., en, es): ').
locale_text(es, language_updated, 'Idioma cambiado a ~w.').
locale_text(es, language_invalid, 'Idioma no admitido.').
locale_text(es, language_options_header, 'Idiomas disponibles:').
locale_text(es, natural_language_offer, '¿Desea describir el problema con sus propias palabras primero? (yes./no.) ').
locale_text(es, natural_language_prompt, 'Describa el problema (termine con un punto): ').
locale_text(es, natural_language_none, 'No se capturó descripción libre. Se continúa con preguntas guiadas.').
locale_text(es, natural_language_summary, 'Síntomas precargados desde la descripción: ~w').
locale_text(es, voice_unavailable, 'Los requisitos de reconocimiento de voz no están instalados. Se usará el modo guiado.').
locale_text(es, voice_prompt, 'Hable sobre el problema después del tono. Presione Enter al finalizar.').
locale_text(es, analytics_header, 'Resumen de analíticas').
locale_text(es, analytics_total_sessions, 'Sesiones totales: ~d').
locale_text(es, analytics_top_causes, 'Causas principales: ~w').
locale_text(es, followup_overheating_loud_fan, 'Se detectó sobrecalentamiento. ¿Los ventiladores hacen ruido inusual?').
locale_text(es, followup_overheating_dust, 'Se detectó sobrecalentamiento. ¿La máquina tiene polvo notable en su interior?').
locale_text(es, followup_slow_performance_disk_full, 'Se observó lentitud. ¿El disco está casi lleno?').
locale_text(es, followup_wifi_disconnects_router, 'Se detectaron cortes de Wi-Fi. ¿El router se reinicia o pierde señal con frecuencia?').
locale_text(es, followup_frequent_crashes_blue_screen, 'Se reportaron fallos frecuentes. ¿Aparecen pantallas azules?').
locale_text(es, followup_display_artifacts_gpu_heat, 'Se detectaron artefactos visuales. ¿La GPU está funcionando caliente?').

localized_text(Key, Args, Text) :-
    current_locale(Locale),
    ( locale_text(Locale, Key, Format)
    -> true
    ;  locale_text(en, Key, Format)
    ),
    format(string(Text), Format, Args).

ui_line(Attrs, Key, Args) :-
    diagnosis_mode(interactive),
    !,
    localized_text(Key, Args, Text),
    ansi_format(Attrs, '~w~n', [Text]).
ui_line(_Attrs, _Key, _Args).

ui_line(Key, Args) :-
    ui_line([fg(cyan)], Key, Args).

ui_notice(Key, Args) :-
    ui_line([bold, fg(green)], Key, Args).

ui_warning(Key, Args) :-
    ui_line([bold, fg(red)], Key, Args).

ui_muted(Key, Args) :-
    ui_line([fg(blue)], Key, Args).

ui_prompt(Key, Args) :-
    diagnosis_mode(interactive),
    !,
    localized_text(Key, Args, Text),
    ansi_format([fg(yellow)], '~w', [Text]),
    flush_output.
ui_prompt(_Key, _Args).

print_menu_option(Index, Key) :-
    diagnosis_mode(interactive),
    !,
    localized_text(Key, [], Text),
    ansi_format([fg(yellow)], '~d. ', [Index]),
    ansi_format([fg(white)], '~w~n', [Text]).
print_menu_option(_, _).

set_diagnosis_mode(Mode) :-
    retractall(diagnosis_mode(_)),
    assertz(diagnosis_mode(Mode)).

init_persistence :-
    with_autosave_suspended(
        (kb_autosave_file(File),
         (exists_file(File) ->
             load_learned_facts(File)
         ;   true))).

with_autosave_suspended(Goal) :-
    retractall(kb_autosave_suspended(_)),
    assertz(kb_autosave_suspended(true)),
    call_cleanup(Goal,
        (retractall(kb_autosave_suspended(_)),
         assertz(kb_autosave_suspended(false)))).

load_learned_facts(File) :-
    open(File, read, Stream),
    call_cleanup(load_terms(Stream), close(Stream)).

load_terms(Stream) :-
    read_term(Stream, Term, []),
    ( Term == end_of_file ->
        true
    ;   ensure_loaded_fact(Term),
        load_terms(Stream)
    ).

ensure_loaded_fact(Fact) :-
    (call(Fact) -> true ; assertz(Fact)),
    record_learnt_fact(Fact).

record_learnt_fact(Fact) :-
    (var(Fact) -> !, fail ; true),
    ( Fact = solution(Cause, _) ->
        retractall(learned_fact(solution(Cause, _)))
    ; Fact = confidence(Cause, _) ->
        retractall(learned_fact(confidence(Cause, _)))
    ; true
    ),
    (learned_fact(Fact) -> true ; assertz(learned_fact(Fact))),
    autosave_kb.

autosave_kb :-
    knowledge_autosave_enabled(true),
    kb_autosave_suspended(false),
    kb_autosave_file(File),
    save_learned_facts(File).
autosave_kb.

save_learned_facts(File) :-
    findall(Fact, learned_fact(Fact), Facts),
    ( Facts = [] ->
        true
    ;   file_directory_name(File, Dir),
        make_directory_path(Dir),
        open(File, write, Stream),
        call_cleanup(write_learned_facts(Stream, Facts), close(Stream))
    ).

write_learned_facts(Stream, Facts) :-
    get_time(Stamp),
    format_time(string(TimeString), '%FT%T%:z', Stamp),
    format(Stream, '%% Auto-generated knowledge snapshot (~w)~n', [TimeString]),
    forall(member(Fact, Facts), portray_clause(Stream, Fact)).

:- initialization(init_persistence, now).

% Initial knowledge
symptom(slow_performance).
symptom(frequent_crashes).
symptom(blue_screen).
symptom(strange_noises).
symptom(overheating).
symptom(unresponsive_programs).
symptom(slow_internet).
symptom(popups).
symptom(boot_failures).
symptom(screen_flickering).
symptom(mouse_lag).
symptom(frequent_restarts).
symptom(application_freezing).
symptom(no_sound).
symptom(unexpected_shutdowns).
symptom(normal_boot).
symptom(sound_works).
symptom(high_performance).
symptom(clean_desktop).
symptom(cool_temperatures).
symptom(wifi_disconnects).
symptom(battery_not_charging).
symptom(loud_fan).
symptom(disk_full).
symptom(update_failures).
symptom(visible_dust).
symptom(display_artifacts).
symptom(startup_beeps).
symptom(network_latency_spikes).

cause(virus).
cause(hardware_failure).
cause(software_corruption).
cause(malware).
cause(driver_issue).
cause(hard_drive_failure).
cause(ram_issue).
cause(overheating).
cause(power_supply_issue).
cause(network_problem).
cause(os_corruption).
cause(too_many_programs_running).
cause(outdated_software).
cause(incompatible_software).
cause(dust).
cause(fan_failure).
cause(system_healthy).
cause(router_issue).
cause(battery_failure).
cause(storage_full).
cause(update_service_issue).
cause(gpu_overheating).
cause(bios_misconfiguration).
cause(isp_throttling).

solution(virus, "Run a full system scan with updated antivirus software.").
solution(hardware_failure, "Check hardware connections and consider replacing faulty components.").
solution(software_corruption, "Reinstall the affected software or reset it to default settings.").
solution(malware, "Use anti-malware software to detect and remove malicious programs.").
solution(driver_issue, "Update or reinstall device drivers.").
solution(hard_drive_failure, "Back up data and replace the hard drive.").
solution(ram_issue, "Test RAM modules and replace if necessary.").
solution(overheating, "Ensure proper ventilation, clean fans, and reduce workload to prevent overheating.").
solution(power_supply_issue, "Check the power supply and replace it if faulty.").
solution(network_problem, "Check network connections and router settings.").
solution(os_corruption, "Repair or reinstall the operating system.").
solution(too_many_programs_running, "Close unnecessary programs and free up system resources.").
solution(outdated_software, "Update all software to the latest versions.").
solution(incompatible_software, "Remove or find alternatives for incompatible software.").
solution(dust, "Clean the computer internals, especially fans and heat sinks.").
solution(fan_failure, "Replace the malfunctioning fan and monitor temperatures.").
solution(system_healthy, "System appears healthy; monitor for future anomalies but no action required.").
solution(router_issue, "Restart the router, update firmware, or contact the ISP if outages persist.").
solution(battery_failure, "Run battery diagnostics and replace the battery if health is critically low.").
solution(storage_full, "Delete unnecessary files or extend storage capacity to free space.").
solution(update_service_issue, "Reset the update service and install pending patches manually.").
solution(gpu_overheating, "Improve GPU cooling, clean vents, and consider reapplying thermal paste.").
solution(bios_misconfiguration, "Reset BIOS/UEFI settings to default or update firmware if available.").
solution(isp_throttling, "Contact the ISP about throttling or schedule usage during off-peak hours.").

symptom_cause(slow_performance, virus).
symptom_cause(slow_performance, hardware_failure).
symptom_cause(slow_performance, too_many_programs_running).
symptom_cause(frequent_crashes, software_corruption).
symptom_cause(frequent_crashes, hardware_failure).
symptom_cause(blue_screen, driver_issue).
symptom_cause(blue_screen, hardware_failure).
symptom_cause(strange_noises, hard_drive_failure).
symptom_cause(strange_noises, fan_failure).
symptom_cause(overheating, dust).
symptom_cause(overheating, fan_failure).
symptom_cause(unresponsive_programs, virus).
symptom_cause(unresponsive_programs, ram_issue).
symptom_cause(slow_internet, network_problem).
symptom_cause(slow_internet, malware).
symptom_cause(popups, malware).
symptom_cause(boot_failures, os_corruption).
symptom_cause(boot_failures, hard_drive_failure).
symptom_cause(screen_flickering, driver_issue).
symptom_cause(screen_flickering, hardware_failure).
symptom_cause(mouse_lag, driver_issue).
symptom_cause(mouse_lag, hardware_failure).
symptom_cause(frequent_restarts, power_supply_issue).
symptom_cause(frequent_restarts, overheating).
symptom_cause(application_freezing, software_corruption).
symptom_cause(application_freezing, ram_issue).
symptom_cause(no_sound, driver_issue).
symptom_cause(unexpected_shutdowns, power_supply_issue).
symptom_cause(unexpected_shutdowns, overheating).
symptom_cause(normal_boot, system_healthy).
symptom_cause(sound_works, system_healthy).
symptom_cause(high_performance, system_healthy).
symptom_cause(clean_desktop, system_healthy).
symptom_cause(cool_temperatures, system_healthy).
symptom_cause(wifi_disconnects, router_issue).
symptom_cause(wifi_disconnects, network_problem).
symptom_cause(battery_not_charging, battery_failure).
symptom_cause(loud_fan, dust).
symptom_cause(loud_fan, fan_failure).
symptom_cause(disk_full, storage_full).
symptom_cause(update_failures, update_service_issue).
symptom_cause(update_failures, os_corruption).
symptom_cause(visible_dust, dust).
symptom_cause(display_artifacts, gpu_overheating).
symptom_cause(display_artifacts, driver_issue).
symptom_cause(startup_beeps, bios_misconfiguration).
symptom_cause(startup_beeps, ram_issue).
symptom_cause(network_latency_spikes, isp_throttling).
symptom_cause(network_latency_spikes, network_problem).

confidence(virus, 0.9).
confidence(hardware_failure, 0.7).
confidence(software_corruption, 0.5).
confidence(malware, 0.8).
confidence(driver_issue, 0.6).
confidence(hard_drive_failure, 0.7).
confidence(ram_issue, 0.6).
confidence(overheating, 0.8).
confidence(power_supply_issue, 0.8).
confidence(network_problem, 0.5).
confidence(os_corruption, 0.7).
confidence(too_many_programs_running, 0.6).
confidence(outdated_software, 0.5).
confidence(incompatible_software, 0.5).
confidence(dust, 0.6).
confidence(fan_failure, 0.7).
confidence(system_healthy, 0.2).
confidence(router_issue, 0.5).
confidence(battery_failure, 0.6).
confidence(storage_full, 0.7).
confidence(update_service_issue, 0.5).
confidence(gpu_overheating, 0.7).
confidence(bios_misconfiguration, 0.5).
confidence(isp_throttling, 0.4).

confidence_weight(yes, 1.0).
confidence_weight(no, -0.8).
confidence_weight(unsure, 0.1).
confidence_weight(none, 0).

symptom_probability(slow_performance, virus, 0.7).
symptom_probability(popups, malware, 0.85).
symptom_probability(overheating, dust, 0.6).
symptom_probability(overheating, fan_failure, 0.65).
symptom_probability(loud_fan, fan_failure, 0.75).
symptom_probability(loud_fan, dust, 0.55).
symptom_probability(blue_screen, driver_issue, 0.8).
symptom_probability(boot_failures, os_corruption, 0.7).
symptom_probability(wifi_disconnects, router_issue, 0.7).
symptom_probability(disk_full, storage_full, 0.9).
symptom_probability(update_failures, update_service_issue, 0.8).
symptom_probability(battery_not_charging, battery_failure, 0.85).
symptom_probability(visible_dust, dust, 0.75).
symptom_probability(display_artifacts, gpu_overheating, 0.7).
symptom_probability(display_artifacts, driver_issue, 0.6).
symptom_probability(startup_beeps, bios_misconfiguration, 0.65).
symptom_probability(startup_beeps, ram_issue, 0.6).
symptom_probability(network_latency_spikes, isp_throttling, 0.7).
symptom_probability(network_latency_spikes, network_problem, 0.55).

% Contradictory symptom pairs
contradiction(overheating, cool_temperatures).
contradiction(cool_temperatures, overheating).
contradiction(boot_failures, normal_boot).
contradiction(normal_boot, boot_failures).
contradiction(no_sound, sound_works).
contradiction(sound_works, no_sound).
contradiction(slow_performance, high_performance).
contradiction(high_performance, slow_performance).
contradiction(popups, clean_desktop).
contradiction(clean_desktop, popups).

symptom_alias(slow_performance, ["slow performance", "lag", "lagging", "sluggish"]).
symptom_alias(frequent_crashes, ["crash", "crashes", "crashing"]).
symptom_alias(blue_screen, ["blue screen", "bsod"]).
symptom_alias(strange_noises, ["strange noise", "weird noise", "clicking", "grinding"]).
symptom_alias(overheating, ["overheat", "too hot", "hot laptop", "fan loud"]).
symptom_alias(unresponsive_programs, ["not responding", "frozen app", "program freeze"]).
symptom_alias(slow_internet, ["slow internet", "slow wifi", "laggy internet"]).
symptom_alias(popups, ["pop up", "ads", "adware", "popup"]).
symptom_alias(boot_failures, ["won't boot", "boot fail", "no boot", "startup fail"]).
symptom_alias(screen_flickering, ["screen flicker", "flickering display"]).
symptom_alias(mouse_lag, ["mouse lag", "cursor lag", "cursor delay"]).
symptom_alias(frequent_restarts, ["keeps restarting", "random restart", "reboots"]).
symptom_alias(display_artifacts, ["visual artifacts", "artifacting", "gpu artifacts", "green pixels"]).
symptom_alias(startup_beeps, ["beeping", "post beeps", "bios beeps"]).
symptom_alias(network_latency_spikes, ["latency spikes", "high ping", "lag spikes"]).
symptom_alias(application_freezing, ["app freeze", "program hang", "freezing app"]).
symptom_alias(no_sound, ["no sound", "muted audio", "sound not working"]).
symptom_alias(unexpected_shutdowns, ["shutdown", "power off", "turns off"]).
symptom_alias(wifi_disconnects, ["wifi drop", "disconnects", "wifi disconnect"]).
symptom_alias(battery_not_charging, ["battery not charging", "plugged not charging"]).
symptom_alias(loud_fan, ["loud fan", "noisy fan", "fan noise"]).
symptom_alias(disk_full, ["disk full", "storage full", "no space"]).
symptom_alias(update_failures, ["update failed", "update error", "windows update"]).
symptom_alias(visible_dust, ["dust buildup", "dusty", "dust inside"]).

followup_rule(overheating, loud_fan, followup_overheating_loud_fan).
followup_rule(overheating, visible_dust, followup_overheating_dust).
followup_rule(slow_performance, disk_full, followup_slow_performance_disk_full).
followup_rule(wifi_disconnects, slow_internet, followup_wifi_disconnects_router).
followup_rule(frequent_crashes, blue_screen, followup_frequent_crashes_blue_screen).
followup_rule(display_artifacts, overheating, followup_display_artifacts_gpu_heat).
% Main program
main :-
    repeat,
    nl,
    display_main_menu,
    read_menu_choice(Choice),
    (   handle_choice(Choice)
    ->  (Choice == 12 -> ! ; fail)
    ;   ui_warning(invalid_choice, []), fail
    ).

display_main_menu :-
    ui_notice(menu_title, []),
    print_menu_option(1, menu_option_1),
    print_menu_option(2, menu_option_2),
    print_menu_option(3, menu_option_3),
    print_menu_option(4, menu_option_4),
    print_menu_option(5, menu_option_5),
    print_menu_option(6, menu_option_6),
    print_menu_option(7, menu_option_7),
    print_menu_option(8, menu_option_8),
    print_menu_option(9, menu_option_9),
    print_menu_option(10, menu_option_10),
    print_menu_option(11, menu_option_11),
    print_menu_option(12, menu_option_12).

% Handle user choices
handle_choice(1) :- start_diagnosis(guided), !.
handle_choice(2) :- start_diagnosis(natural_language), !.
handle_choice(3) :- start_diagnosis(voice), !.
handle_choice(4) :- learn_symptom, !.
handle_choice(5) :- learn_cause, !.
handle_choice(6) :- learn_solution, !.
handle_choice(7) :- view_knowledge_base, !.
handle_choice(8) :- save_knowledge, !.
handle_choice(9) :- load_knowledge, !.
handle_choice(10) :- show_analytics_summary, !.
handle_choice(11) :- change_language, !.
handle_choice(12) :- ui_notice(menu_option_12, []), !.
handle_choice(_) :- fail.

read_menu_choice(Choice) :-
    read_numeric_choice(ui_prompt(menu_prompt, []), 1, 12, Choice).

read_numeric_choice(PromptGoal, Min, Max, Choice) :-
    call(PromptGoal),
    flush_output,
    read_line_to_string(user_input, Raw),
    (   parse_numeric_choice(Raw, Min, Max, Choice)
    ->  true
    ;   ui_warning(invalid_choice, []),
        read_numeric_choice(PromptGoal, Min, Max, Choice)
    ).

parse_menu_choice(Raw, Choice) :-
    parse_numeric_choice(Raw, 1, 12, Choice).

parse_numeric_choice(Raw, Min, Max, Choice) :-
    normalize_space(string(Trimmed), Raw),
    Trimmed \= "",
    strip_trailing_period(Trimmed, CleanString),
    atom_string(Atom, CleanString),
    catch(read_term_from_atom(Atom, Parsed, []), _, fail),
    integer(Parsed),
    Parsed >= Min,
    Parsed =< Max,
    Choice = Parsed.

strip_trailing_period(String, Cleaned) :-
    (   sub_string(String, Before, 1, 0, "."),
        string_length(String, Len),
        Len1 is Len - 1,
        Before =:= Len1
    ->  sub_string(String, 0, Before, _, Cleaned)
    ;   Cleaned = String
    ).

% Start the diagnosis process
start_diagnosis :-
    start_diagnosis(guided).

start_diagnosis(Mode) :-
    set_diagnosis_mode(interactive),
    ui_notice(welcome_message, []),
    ui_line([fg(magenta)], input_instruction, []),
    collect_prefilled_answers(Mode, PrefilledAnswers),
    findall(Symptom, (symptom(Symptom), Symptom \= unexpected_shutdowns), Symptoms),
    ask_symptoms(Symptoms, PrefilledAnswers, InitialAnswers),
    apply_followup_rules(InitialAnswers, RawAnswers),
    resolve_contradictions(RawAnswers, AllAnswers),
    find_causes(AllAnswers, Causes),
    present_diagnosis(Causes, AllAnswers),
    !.  % Cut to prevent backtracking

collect_prefilled_answers(guided, Prefilled) :-
    ui_prompt(natural_language_offer, []),
    read(Response),
    (Response == yes ->
        capture_free_text(Prefilled)
    ;   Prefilled = []
    ).
collect_prefilled_answers(natural_language, Prefilled) :-
    capture_free_text(Prefilled).
collect_prefilled_answers(voice, Prefilled) :-
    capture_voice_text(Text),
    prefill_from_text(Text, Prefilled).

capture_free_text(Prefilled) :-
    ui_prompt(natural_language_prompt, []),
    flush_output,
    read_line_to_string(user_input, Description),
    prefill_from_text(Description, Prefilled).

capture_voice_text(Text) :-
    localized_text(voice_prompt, [], Prompt),
    ansi_format([bold, fg(yellow)], '~w~n', [Prompt]),
    (   fetch_voice_text(VoiceText)
    ->  Text = VoiceText
    ;   ui_warning(voice_unavailable, []),
        Text = ''
    ).

prefill_from_text(Text, Prefilled) :-
    string_lower(Text, Lower),
    findall((Symptom, yes), (
        symptom_alias(Symptom, Aliases),
        alias_matches(Lower, Aliases)
    ), Matches),
    list_to_set(Matches, Set),
    (   Set = []
    ->  ui_muted(natural_language_none, []),
        Prefilled = []
    ;   findall(S, member((S, _), Set), SummarySymptoms),
        ui_muted(natural_language_summary, [SummarySymptoms]),
        Prefilled = Set
    ).

alias_matches(_, []) :- fail.
alias_matches(Text, [Alias|_]) :- sub_string(Text, _, _, _, Alias), !.
alias_matches(Text, [_|Rest]) :- alias_matches(Text, Rest).

ask_symptoms(Symptoms, PrefilledRaw, Answers) :-
    include(valid_prefill(Symptoms), PrefilledRaw, Filtered),
    list_to_set(Filtered, Prefilled),
    ask_symptoms_loop(Symptoms, Prefilled, Collected),
    reverse(Collected, Answers).

valid_prefill(Symptoms, (Symptom, _)) :- member(Symptom, Symptoms).

ask_symptoms_loop([], Acc, Acc).
ask_symptoms_loop([Symptom|Rest], Acc, Answers) :-
    (   member((Symptom, _Existing), Acc)
    ->  NextAcc = Acc
    ;   ask_symptom(Symptom, Answer),
        NextAcc = [(Symptom, Answer)|Acc]
    ),
    ask_symptoms_loop(Rest, NextAcc, Answers).

ask_symptom(_Symptom, Answer) :-
    diagnosis_mode(non_interactive),
    !,
    Answer = unsure.

ask_symptom(Symptom, Answer) :-
    format(atom(Question), 'Does the computer exhibit ~w (yes./no./unsure.) ', [Symptom]),
    repeat,
    ansi_format([fg(magenta)], '~w', [Question]),
    read(UserAnswer),
    (member(UserAnswer, [yes, no, unsure]) ->
        Answer = UserAnswer;
    ui_warning(invalid_choice, []), fail).

resolve_contradictions(Answers, NormalizedAnswers) :-
    (   find_conflict_pair(Answers, A, B)
    ->  ui_warning(contradiction_notice, [A, B]),
        prompt_conflict_resolution(A, B, Answers, Updated),
        resolve_contradictions(Updated, NormalizedAnswers)
    ;   NormalizedAnswers = Answers
    ).

apply_followup_rules(Answers, FinalAnswers) :-
    apply_followup_rules_iter(Answers, FinalAnswers).

apply_followup_rules_iter(Current, Final) :-
    findall(rule(Symptom, Next, Key),
        ( member((Symptom, yes), Current),
          followup_rule(Symptom, Next, Key),
          \+ member((Next, _), Current)
        ), Rules),
    (   Rules = []
    ->  Final = Current
    ;   ask_followup_rules(Rules, Current, Updated),
        apply_followup_rules_iter(Updated, Final)
    ).

ask_followup_rules([], Acc, Acc).
ask_followup_rules([rule(_, Next, Key)|Rest], Acc, Final) :-
    (   member((Next, _), Acc)
    ->  Updated = Acc
    ;   ui_line([fg(yellow)], Key, []),
        ask_symptom(Next, Answer),
        Updated = [(Next, Answer)|Acc]
    ),
    ask_followup_rules(Rest, Updated, Final).

find_conflict_pair(Answers, A, B) :-
    contradiction(A, B),
    member((A, yes), Answers),
    member((B, yes), Answers).

prompt_conflict_resolution(A, B, Answers, Updated) :-
    diagnosis_mode(non_interactive),
    !,
    % For non-interactive (API) mode, always mark both as unsure, no output
    adjust_answers(A, unsure, Answers, Temp),
    adjust_answers(B, unsure, Temp, Updated).

prompt_conflict_resolution(A, B, Answers, Updated) :-
    ui_line([fg(yellow)], contradiction_resolution_prompt, []),
    localized_text(contradiction_option_1, [A], Opt1),
    localized_text(contradiction_option_2, [B], Opt2),
    localized_text(contradiction_option_3, [], Opt3),
    ansi_format([fg(yellow)], '1. ~w~n', [Opt1]),
    ansi_format([fg(yellow)], '2. ~w~n', [Opt2]),
    ansi_format([fg(yellow)], '3. ~w~n', [Opt3]),
    read_numeric_choice(ui_prompt(conflict_choice_prompt, []), 1, 3, Choice),
    (   Choice == 1 ->
        adjust_answers(B, no, Answers, Updated)
    ;   Choice == 2 ->
        adjust_answers(A, no, Answers, Updated)
    ;   Choice == 3 ->
        adjust_answers(A, unsure, Answers, Temp),
        adjust_answers(B, unsure, Temp, Updated)
    ).

adjust_answers(Symptom, NewAnswer, [(Symptom, _)|Rest], [(Symptom, NewAnswer)|Rest]) :- !.
adjust_answers(Symptom, NewAnswer, [Pair|Rest], [Pair|Updated]) :-
    adjust_answers(Symptom, NewAnswer, Rest, Updated).

% Non-interactive diagnosis helper for automated tests
diagnosis_from_answers(InitialAnswers, Causes, FinalAnswers) :-
    setup_call_cleanup(
        set_diagnosis_mode(non_interactive),
        (   apply_followup_rules(InitialAnswers, Augmented),
            resolve_contradictions(Augmented, FinalAnswers),
            find_causes(FinalAnswers, Causes)
        ),
        set_diagnosis_mode(interactive)
    ).

% Find potential causes based on observed symptoms
find_causes(AllAnswers, Causes) :-
    findall(Cause, (
        member((Symptom, _), AllAnswers),
        symptom_cause(Symptom, Cause)
    ), CausesWithDuplicates),
    sort(CausesWithDuplicates, UniqueCauses),
    (UniqueCauses == [] ->
        Causes = [(unknown, 1.0)];
        findall((Cause, FinalConf), (
            member(Cause, UniqueCauses),
            adjust_confidence(Cause, AllAnswers, WeightedConf),
            bayesian_adjustment(Cause, AllAnswers, WeightedConf, BayesianConf),
            combine_confidence(WeightedConf, BayesianConf, FinalConf)
        ), CausesWithConf),
        sort(2, @>=, CausesWithConf, Causes)
    ).

% Adjust confidence based on the answers to symptoms
adjust_confidence(Cause, AllAnswers, AdjustedConf) :-
    confidence(Cause, BaseConf),
    findall(Symptom, symptom_cause(Symptom, Cause), SymptomsForCause),
    length(SymptomsForCause, NumSymptoms),
    (NumSymptoms == 0 ->
        AdjustedConf = BaseConf
    ;   calculate_weighted_score(SymptomsForCause, AllAnswers, Score, EvidenceCount),
        EvidenceFactor is max(1, EvidenceCount),
        NormalizedScore is Score / EvidenceFactor,
        SaturatedScore is min(1.0, max(-1.0, NormalizedScore)),
        AdjustedConf is max(0.0, min(1.0, BaseConf + (SaturatedScore * (BaseConf))))
    ).

bayesian_adjustment(Cause, AllAnswers, WeightedConf, BayesianConf) :-
    confidence(Cause, Prior),
    findall(Factor, bayesian_factor(Cause, AllAnswers, Factor), Factors),
    (   Factors = []
    ->  BayesianConf = WeightedConf
    ;   product_list(Factors, Product),
        Raw is Prior * Product,
        clamp01(Raw, BayesianConf)
    ).

bayesian_factor(Cause, AllAnswers, Factor) :-
    member((Symptom, Response), AllAnswers),
    symptom_probability(Symptom, Cause, Likelihood),
    response_factor(Response, Likelihood, Factor).

response_factor(yes, Likelihood, Factor) :-
    Factor is max(0.01, Likelihood).
response_factor(no, Likelihood, Factor) :-
    Complement is 1.0 - Likelihood,
    Factor is max(0.01, Complement).
response_factor(unsure, _, 0.5).
response_factor(_, _, 1.0).

combine_confidence(Weighted, Bayesian, Final) :-
    Blended is (Weighted * 0.6) + (Bayesian * 0.4),
    clamp01(Blended, Final).

product_list([], 1.0).
product_list([H|T], Product) :-
    product_list(T, Rest),
    Product is H * Rest.

clamp01(Value, 1.0) :- Value > 1.0, !.
clamp01(Value, 0.0) :- Value < 0.0, !.
clamp01(Value, Value).

% Calculate weighted score based on answers
calculate_weighted_score([], _, 0, 0).
calculate_weighted_score([Symptom|Rest], AllAnswers, Score, EvidenceCount) :-
    (   member((Symptom, Answer), AllAnswers)
    ->  confidence_weight(Answer, Weight)
    ;   Weight = 0,
        Answer = none
    ),
    (Answer == none -> EvidenceInc = 0 ; EvidenceInc = 1),
    calculate_weighted_score(Rest, AllAnswers, RestScore, RestEvidence),
    Score is RestScore + Weight,
    EvidenceCount is RestEvidence + EvidenceInc.

% Present the diagnosis results with added error handling and redundancy removal
present_diagnosis(Causes, Answers) :-
    ansi_format([bold, fg(green)], 'Based on the symptoms, the problem(s) might be:~n', []),
    present_causes(Causes),
    print_session_summary(Answers),
    log_session(Causes, Answers),
    ask_explanation(Causes).

% Present causes and their solutions
present_causes([]).
present_causes([(unknown, _)|_]) :-
    write('No known cause matches the observed symptoms.'), nl, !.
present_causes([(Cause, Conf)|Rest]) :-
    ansi_format([fg(cyan)], '~w (Confidence: ~2f): ', [Cause, Conf]),
    (solution(Cause, Solution) ->
        ansi_format([fg(white)], 'Solution: ~w~n', [Solution])
    ;   ansi_format([fg(white)], 'No specific solution available.~n', [])
    ),
    present_causes(Rest).

print_session_summary(Answers) :-
    include(answer_yes, Answers, Positives),
    include(answer_no, Answers, Negatives),
    include(answer_unsure, Answers, Unsures),
    ui_notice(summary_title, []),
    print_summary_line(summary_positive, Positives),
    print_summary_line(summary_negative, Negatives),
    print_summary_line(summary_unsure, Unsures).

answer_yes((_, yes)).
answer_no((_, no)).
answer_unsure((_, unsure)).

print_summary_line(_, []) :-
    ui_muted(summary_none, []).
print_summary_line(Key, Items) :-
    length(Items, Count),
    findall(S, member((S, _), Items), Symbols),
    ui_line([fg(white)], Key, [Count, Symbols]).

log_session(Causes, Answers) :-
    build_session_payload(Causes, Answers, Payload),
    catch(append_session_payload(Payload), Error,
        (print_message(error, Error), true)).

build_session_payload(Causes, Answers, _{timestamp: TimeString,
                                        positives: Positives,
                                        negatives: Negatives,
                                        unsures: Unsures,
                                        diagnoses: DiagnosisList}) :-
    get_time(Stamp),
    format_time(string(TimeString), '%FT%T%:z', Stamp),
    findall(S, member((S, yes), Answers), Positives),
    findall(S, member((S, no), Answers), Negatives),
    findall(S, member((S, unsure), Answers), Unsures),
    maplist(cause_dict, Causes, DiagnosisList).

cause_dict((Cause, Confidence), _{cause: Cause, confidence: Confidence}).

append_session_payload(Payload) :-
    analytics_file(File),
    file_directory_name(File, Dir),
    make_directory_path(Dir),
    open(File, append, Stream),
    call_cleanup(
        (json_write_dict(Stream, Payload, [width(0)]), nl(Stream), ui_muted(analytics_logged, [])),
        close(Stream)).

show_analytics_summary :-
    analytics_file(File),
    (   exists_file(File)
    ->  read_analytics_records(File, Records),
        (   Records == []
        ->  ui_muted(analytics_none, [])
        ;   length(Records, Count),
            gather_top_causes(Records, TopCauses),
            ui_notice(analytics_header, []),
            ui_line([fg(white)], analytics_total_sessions, [Count]),
            ui_line([fg(white)], analytics_top_causes, [TopCauses])
        )
    ;   ui_muted(analytics_none, [])
    ).

read_analytics_records(File, Records) :-
    open(File, read, Stream),
    call_cleanup(read_analytics_stream(Stream, Records), close(Stream)).

read_analytics_stream(Stream, Records) :-
    read_line_to_string(Stream, Line),
    (   Line == end_of_file
    ->  Records = []
    ;   Line = ""
    ->  read_analytics_stream(Stream, Records)
    ;   atom_string(Atom, Line),
        atom_json_term(Atom, Dict, []),
        read_analytics_stream(Stream, Rest),
        Records = [Dict|Rest]
    ).

gather_top_causes(Records, TopCauses) :-
    findall(Cause,
        ( member(Record, Records),
          get_dict(diagnoses, Record, Diagnoses),
          member(Dict, Diagnoses),
          get_dict(cause, Dict, Cause)
        ), Causes),
    (   Causes == []
    ->  TopCauses = []
    ;   count_occurrences(Causes, Counts),
        sort(2, @>=, Counts, Sorted),
        take(3, Sorted, TopCauses)
    ).

count_occurrences(List, Counts) :-
    findall(Item-1, member(Item, List), Pairs),
    keysort(Pairs, Sorted),
    group_pairs_by_key(Sorted, Grouped),
    maplist(sum_pair, Grouped, Counts).

sum_pair(Key-Values, Key-Count) :- sum_list(Values, Count).

take(_, [], []).
take(0, _, []).
take(N, [H|T], [H|Rest]) :-
    N > 0,
    N1 is N - 1,
    take(N1, T, Rest).

change_language :-
    ui_line([fg(white)], language_options_header, []),
    forall(supported_locale(Code, Name), ansi_format([fg(yellow)], '~w (~w)~n', [Code, Name])),
    ui_prompt(language_prompt, []),
    flush_output,
    read_line_to_string(user_input, Raw),
    (   Raw == ""
    ->  ui_warning(language_invalid, [])
    ;   atom_string(Atom, Raw),
        downcase_atom(Atom, Locale),
        ( supported_locale(Locale, Name) ->
            retractall(current_locale(_)),
            assertz(current_locale(Locale)),
            ui_notice(language_updated, [Name])
        ;   ui_warning(language_invalid, [])
        )
    ).

fetch_voice_text(Text) :-
    voice_script(Script),
    exists_file(Script),
    !,
    catch(run_voice_script(Script, Text), _, fail).
fetch_voice_text(_) :- fail.

run_voice_script(Script, Text) :-
    process_create(path(python3), [Script], [stdout(pipe(Out)), stderr(pipe(Err)), process(PID)]),
    call_cleanup(
        ( read_string(Out, _, Raw),
                    normalize_space(atom(TextAtom), Raw),
                    atom_string(TextAtom, Text),
          read_string(Err, _, _)
        ),
        ( close(Out), close(Err), process_wait(PID, _) )
    ),
    Text \= ''.

% Offer an explanation of the diagnosis
ask_explanation(Causes) :-
    nl, write('Would you like an explanation of the diagnosis? (yes./no.) '),
    read(Answer),
    (Answer == yes ->
        explain_diagnosis(Causes);
    true),
    !.  % Cut to prevent backtracking

% Explain the diagnosis process
explain_diagnosis(Causes) :-
    nl, write('The system uses the following logic for diagnosis:'), nl,
    write('1. It asks about known symptoms, allowing for uncertainty.'), nl,
    write('2. It matches symptoms to possible causes based on confidence levels.'), nl,
    write('3. It adjusts confidence based on symptom answers (yes +1, no -0.5, unsure 0), normalized by number of associated symptoms.'), nl,
    write('4. If multiple causes are likely, it prioritizes them based on adjusted confidence.'), nl,
    nl, write('Here are the specific symptoms and their likely causes:'), nl,
    explain_causes(Causes),
    !.  % Cut to prevent backtracking

% Explain individual causes
explain_causes([]).
explain_causes([(unknown, _)|_]) :-
    write('No further explanation is available because no matching cause was found.'), nl, !.
explain_causes([(Cause, Conf)|Rest]) :-
    format('Cause: ~w, Adjusted Confidence: ~2f~n', [Cause, Conf]),
    (solution(Cause, Solution) ->
        format('Solution: ~w~n', [Solution]);
    write('No specific solution available.')), nl,
    explain_causes(Rest).

% Learn a new symptom
learn_symptom :-
    write('Enter a new symptom: '), nl,
    read(NewSymptom),
    (symptom(NewSymptom) ->
        write('This symptom already exists in the knowledge base.');
    asserta(symptom(NewSymptom)),
    record_learnt_fact(symptom(NewSymptom)),
    write('New symptom added to the knowledge base.')),
    nl.

% Learn a new cause
learn_cause :-
    write('Enter a new cause: '), nl,
    read(NewCause),
    (cause(NewCause) ->
        write('This cause already exists. Do you want to add a new symptom-cause relationship? (yes./no.) '),
        read(Answer),
        (Answer == yes ->
            add_symptom_cause(NewCause);
        write('No changes made to the knowledge base.'));
    asserta(cause(NewCause)),
    record_learnt_fact(cause(NewCause)),
    write('New cause added. '),
    add_symptom_cause(NewCause)),
    nl.

% Add a symptom-cause relationship
add_symptom_cause(Cause) :-
    write('Enter the related symptom: '), nl,
    read(Symptom),
    (symptom(Symptom) ->
        (symptom_cause(Symptom, Cause) ->
            write('This symptom-cause relationship already exists.');
        asserta(symptom_cause(Symptom, Cause)),
        record_learnt_fact(symptom_cause(Symptom, Cause)),
        write('New cause-symptom relationship added to the knowledge base.'),
        add_confidence(Cause));
    write('This symptom does not exist in the knowledge base. Please add it first.')),
    nl.

% Add confidence for a cause
add_confidence(Cause) :-
    write('Enter confidence level for this cause (0.0 - 1.0): '),
    read(Conf),
    (number(Conf), Conf >= 0.0, Conf =< 1.0 ->
        asserta(confidence(Cause, Conf)),
        record_learnt_fact(confidence(Cause, Conf)),
        write('New confidence level added.');
    write('Invalid confidence level. Please enter a number between 0.0 and 1.0.'),
    add_confidence(Cause)).

% Learn a new solution
learn_solution :-
    write('Enter a cause: '), nl,
    read(Cause),
    atom_string(Cause, CauseString),
    write('Enter the solution: '), nl,
    capture_solution(Solution),
    validate_solution(Solution, CauseString).

% Capture solution input using read_line_to_string/2
capture_solution(Solution) :-
    flush_output,
    read(user_input, Solution),
    format('Debug: Solution entered: "~w"~n', [Solution]).

% Validate the solution
validate_solution(Solution, Cause) :-
    (Solution == '' ->
        write('Empty solution detected. Please try again.'), nl,
        learn_solution;
        format('You entered: "~w"~n', [Solution]),
        write('Is this correct? (yes./no.) '),
        flush_output,
        read(Confirm),
        (Confirm == yes ->
            confirm_solution(Solution, Cause);
            Confirm == no ->
            write('Let\'s try again.'), nl,
            learn_solution;
            write('Invalid input. Please enter "yes" or "no".'), nl,
            validate_solution(Solution, Cause))).

% Clean up and confirm the solution
confirm_solution(Solution, Cause) :-
    atom_string(CauseAtom, Cause),
    atom_string(SolutionAtom, Solution),
    normalize_space(atom(CauseNorm), CauseAtom),
    normalize_space(atom(SolutionNorm), SolutionAtom),
    (solution(CauseNorm, OldSolution) ->
        format('A solution for this cause already exists: ~w~n', [OldSolution]),
        write('Do you want to update it? (yes./no.) '),
        flush_output,
        read(Answer),
        (Answer == yes ->
            update_solution(CauseNorm, SolutionNorm);
        Answer == no ->
            write('No changes made to the knowledge base.'), nl;
        write('Invalid input. Please enter "yes" or "no".'), nl,
        confirm_solution(Solution, Cause));
    assertz(solution(CauseNorm, SolutionNorm)),
    record_learnt_fact(solution(CauseNorm, SolutionNorm)),
    write('New solution added to the knowledge base.'), nl).


% Update the solution
update_solution(Cause, Solution) :-
    retract(solution(Cause, _)),
    assertz(solution(Cause, Solution)),
    record_learnt_fact(solution(Cause, Solution)),
    write('Solution updated in the knowledge base.'), nl.


% view_knowledge_base/0 with options to view specific parts or all sections.
view_knowledge_base :-
    nl,
    % Provide options for the user to view specific parts or the entire knowledge base.
    write('Choose what to view:'), nl,
    write('1. View all'), nl,
    write('2. View symptoms'), nl,
    write('3. View causes'), nl,
    write('4. View symptom-cause relationships'), nl,
    write('5. View confidences'), nl,
    write('6. View solutions'), nl,
    write('7. Exit'), nl,
    read_numeric_choice(ui_prompt(view_kb_prompt, []), 1, 7, Choice),
    handle_view_choice(Choice).

% Handle user's choice based on their input
handle_view_choice(1) :-
    view_all, !.  % View all sections.
handle_view_choice(2) :-
    view_symptoms, !.  % View only symptoms.
handle_view_choice(3) :-
    view_causes, !.  % View only causes.
handle_view_choice(4) :-
    view_symptom_cause, !.  % View only symptom-cause relationships.
handle_view_choice(5) :-
    view_confidences, !.  % View only confidences.
handle_view_choice(6) :-
    view_solutions, !.  % View only solutions.
handle_view_choice(7) :-
    write('Exiting view knowledge base.'), !.  % Exit.
handle_view_choice(_) :-
    write('Invalid option. Please try again.'), nl,
    view_knowledge_base.

% View all sections of the knowledge base in a structured format
view_all :-
    view_symptoms,
    view_causes,
    view_symptom_cause,
    view_confidences,
    view_solutions.

% View only the symptoms, each on a new line
view_symptoms :-
    nl, write('Symptoms:'), nl,
    findall(S, symptom(S), Symptoms),
    print_list(Symptoms).

% View only the causes, each on a new line
view_causes :-
    nl, write('Causes:'), nl,
    findall(C, confidence(C, _), Causes),
    print_list(Causes).

% View symptom-cause relationships in a structured format
view_symptom_cause :-
    nl, write('Symptom-Cause Relationships:'), nl,
    findall((S, C), symptom_cause(S, C), SCPairs),
    print_pairs(SCPairs).

% View the confidence levels associated with each cause
view_confidences :-
    nl, write('Confidences:'), nl,
    findall((C, Conf), confidence(C, Conf), Confidences),
    print_pairs(Confidences).

% View the solutions associated with each cause in a structured format
view_solutions :-
    nl, write('Solutions:'), nl,
    findall((C, Sol), solution(C, Sol), Solutions),
    print_pairs(Solutions).

% Utility to print a list of items, each on a new line
print_list([]).
print_list([H|T]) :-
    write('- '), write(H), nl,
    print_list(T).

% Utility to print pairs of items in a structured format
print_pairs([]).
print_pairs([(A, B)|T]) :-
    write('- '), write(A), write(' -> '), write(B), nl,
    print_pairs(T).

    
% Debug: List all solutions
debug_list_solutions :-
    write('Listing all solutions in the knowledge base:'), nl,
    forall(solution(Cause, Solution),
           format('Cause: ~w, Solution: ~w~n', [Cause, Solution])).
% Save knowledge to a file
save_knowledge :-
    write('Enter filename to save knowledge (e.g., kb_saved.pl): '), nl,
    read(File),
    (atom(File) ->
        save_kb(File),
        format('Knowledge saved to ~w~n', [File]);
        write('Invalid filename. Use an atom like mykb.pl.'), nl, save_knowledge).

% Load knowledge from a file
load_knowledge :-
    write('Enter filename to load knowledge from (e.g., kb_saved.pl): '), nl,
    read(File),
    (atom(File) ->
        (exists_file(File) ->
            consult(File),
            format('Knowledge loaded from ~w~n', [File]);
            format('File ~w not found.~n', [File]));
        write('Invalid filename. Use an atom like mykb.pl.'), nl, load_knowledge).

% Helper to write current dynamic facts to a file
save_kb(File) :-
    open(File, write, Stream),
    with_output_to(Stream, (listing(symptom), listing(cause), listing(solution), listing(symptom_cause), listing(confidence))),
    close(Stream).