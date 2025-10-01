(rule
 (targets affix_j.ml affix_j.mli)
 (deps affix.atd)
 (mode fallback)
 (action
  (run atdgen -j -j-strict-fields -j-std %{deps} -deriving-conv "eq,show")))

(rule
 (targets affix_t.ml affix_t.mli)
 (deps affix.atd)
 (mode fallback)
 (action
  (run atdgen -t %{deps} -deriving-conv "eq,show")))

(rule
 (targets affix_v.ml affix_v.mli)
 (deps affix.atd)
 (mode fallback)
 (action
  (run atdgen -v %{deps} -deriving-conv "eq,show")))

(rule
 (targets animation_j.ml animation_j.mli)
 (deps animation.atd)
 (mode fallback)
 (action
  (run atdgen -j -j-strict-fields -j-std %{deps} -deriving-conv "eq,show")))

(rule
 (targets animation_t.ml animation_t.mli)
 (deps animation.atd)
 (mode fallback)
 (action
  (run atdgen -t %{deps} -deriving-conv "eq,show")))

(rule
 (targets animation_v.ml animation_v.mli)
 (deps animation.atd)
 (mode fallback)
 (action
  (run atdgen -v %{deps} -deriving-conv "eq,show")))

(rule
 (targets audio_j.ml audio_j.mli)
 (deps audio.atd)
 (mode fallback)
 (action
  (run atdgen -j -j-strict-fields -j-std %{deps} -deriving-conv "eq,show")))

(rule
 (targets audio_t.ml audio_t.mli)
 (deps audio.atd)
 (mode fallback)
 (action
  (run atdgen -t %{deps} -deriving-conv "eq,show")))

(rule
 (targets audio_v.ml audio_v.mli)
 (deps audio.atd)
 (mode fallback)
 (action
  (run atdgen -v %{deps} -deriving-conv "eq,show")))

(rule
 (targets character_j.ml character_j.mli)
 (deps character.atd)
 (mode fallback)
 (action
  (run atdgen -j -j-strict-fields -j-std %{deps} -deriving-conv "eq,show")))

(rule
 (targets character_t.ml character_t.mli)
 (deps character.atd)
 (mode fallback)
 (action
  (run atdgen -t %{deps} -deriving-conv "eq,show")))

(rule
 (targets character_v.ml character_v.mli)
 (deps character.atd)
 (mode fallback)
 (action
  (run atdgen -v %{deps} -deriving-conv "eq,show")))

(rule
 (targets common_j.ml common_j.mli)
 (deps common.atd)
 (mode fallback)
 (action
  (run atdgen -j -j-strict-fields -j-std %{deps} -deriving-conv "eq,show")))

(rule
 (targets common_t.ml common_t.mli)
 (deps common.atd)
 (mode fallback)
 (action
  (run atdgen -t %{deps} -deriving-conv "eq,show")))

(rule
 (targets common_v.ml common_v.mli)
 (deps common.atd)
 (mode fallback)
 (action
  (run atdgen -v %{deps} -deriving-conv "eq,show")))

(rule
 (targets cursor_j.ml cursor_j.mli)
 (deps cursor.atd)
 (mode fallback)
 (action
  (run atdgen -j -j-strict-fields -j-std %{deps} -deriving-conv "eq,show")))

(rule
 (targets cursor_t.ml cursor_t.mli)
 (deps cursor.atd)
 (mode fallback)
 (action
  (run atdgen -t %{deps} -deriving-conv "eq,show")))

(rule
 (targets cursor_v.ml cursor_v.mli)
 (deps cursor.atd)
 (mode fallback)
 (action
  (run atdgen -v %{deps} -deriving-conv "eq,show")))

(rule
 (targets drop_j.ml drop_j.mli)
 (deps drop.atd)
 (mode fallback)
 (action
  (run atdgen -j -j-strict-fields -j-std %{deps} -deriving-conv "eq,show")))

(rule
 (targets drop_t.ml drop_t.mli)
 (deps drop.atd)
 (mode fallback)
 (action
  (run atdgen -t %{deps} -deriving-conv "eq,show")))

(rule
 (targets drop_v.ml drop_v.mli)
 (deps drop.atd)
 (mode fallback)
 (action
  (run atdgen -v %{deps} -deriving-conv "eq,show")))

(rule
 (targets entity_j.ml entity_j.mli)
 (deps entity.atd)
 (mode fallback)
 (action
  (run atdgen -j -j-strict-fields -j-std %{deps} -deriving-conv "eq,show")))

(rule
 (targets entity_t.ml entity_t.mli)
 (deps entity.atd)
 (mode fallback)
 (action
  (run atdgen -t %{deps} -deriving-conv "eq,show")))

(rule
 (targets entity_v.ml entity_v.mli)
 (deps entity.atd)
 (mode fallback)
 (action
  (run atdgen -v %{deps} -deriving-conv "eq,show")))

(rule
 (targets equipment_j.ml equipment_j.mli)
 (deps equipment.atd)
 (mode fallback)
 (action
  (run atdgen -j -j-strict-fields -j-std %{deps} -deriving-conv "eq,show")))

(rule
 (targets equipment_t.ml equipment_t.mli)
 (deps equipment.atd)
 (mode fallback)
 (action
  (run atdgen -t %{deps} -deriving-conv "eq,show")))

(rule
 (targets equipment_v.ml equipment_v.mli)
 (deps equipment.atd)
 (mode fallback)
 (action
  (run atdgen -v %{deps} -deriving-conv "eq,show")))

(rule
 (targets image_j.ml image_j.mli)
 (deps image.atd)
 (mode fallback)
 (action
  (run atdgen -j -j-strict-fields -j-std %{deps} -deriving-conv "eq,show")))

(rule
 (targets image_t.ml image_t.mli)
 (deps image.atd)
 (mode fallback)
 (action
  (run atdgen -t %{deps} -deriving-conv "eq,show")))

(rule
 (targets image_v.ml image_v.mli)
 (deps image.atd)
 (mode fallback)
 (action
  (run atdgen -v %{deps} -deriving-conv "eq,show")))

(rule
 (targets model_j.ml model_j.mli)
 (deps model.atd)
 (mode fallback)
 (action
  (run atdgen -j -j-strict-fields -j-std %{deps} -deriving-conv "eq,show")))

(rule
 (targets model_t.ml model_t.mli)
 (deps model.atd)
 (mode fallback)
 (action
  (run atdgen -t %{deps} -deriving-conv "eq,show")))

(rule
 (targets model_v.ml model_v.mli)
 (deps model.atd)
 (mode fallback)
 (action
  (run atdgen -v %{deps} -deriving-conv "eq,show")))

(rule
 (targets quality_j.ml quality_j.mli)
 (deps quality.atd)
 (mode fallback)
 (action
  (run atdgen -j -j-strict-fields -j-std %{deps} -deriving-conv "eq,show")))

(rule
 (targets quality_t.ml quality_t.mli)
 (deps quality.atd)
 (mode fallback)
 (action
  (run atdgen -t %{deps} -deriving-conv "eq,show")))

(rule
 (targets quality_v.ml quality_v.mli)
 (deps quality.atd)
 (mode fallback)
 (action
  (run atdgen -v %{deps} -deriving-conv "eq,show")))

(rule
 (targets requirement_j.ml requirement_j.mli)
 (deps requirement.atd)
 (mode fallback)
 (action
  (run atdgen -j -j-strict-fields -j-std %{deps} -deriving-conv "eq,show")))

(rule
 (targets requirement_t.ml requirement_t.mli)
 (deps requirement.atd)
 (mode fallback)
 (action
  (run atdgen -t %{deps} -deriving-conv "eq,show")))

(rule
 (targets requirement_v.ml requirement_v.mli)
 (deps requirement.atd)
 (mode fallback)
 (action
  (run atdgen -v %{deps} -deriving-conv "eq,show")))

(rule
 (targets skill_j.ml skill_j.mli)
 (deps skill.atd)
 (mode fallback)
 (action
  (run atdgen -j -j-strict-fields -j-std %{deps} -deriving-conv "eq,show")))

(rule
 (targets skill_t.ml skill_t.mli)
 (deps skill.atd)
 (mode fallback)
 (action
  (run atdgen -t %{deps} -deriving-conv "eq,show")))

(rule
 (targets skill_v.ml skill_v.mli)
 (deps skill.atd)
 (mode fallback)
 (action
  (run atdgen -v %{deps} -deriving-conv "eq,show")))

(rule
 (targets stat_j.ml stat_j.mli)
 (deps stat.atd)
 (mode fallback)
 (action
  (run atdgen -j -j-strict-fields -j-std %{deps} -deriving-conv "eq,show")))

(rule
 (targets stat_t.ml stat_t.mli)
 (deps stat.atd)
 (mode fallback)
 (action
  (run atdgen -t %{deps} -deriving-conv "eq,show")))

(rule
 (targets stat_v.ml stat_v.mli)
 (deps stat.atd)
 (mode fallback)
 (action
  (run atdgen -v %{deps} -deriving-conv "eq,show")))

(rule
 (targets status_j.ml status_j.mli)
 (deps status.atd)
 (mode fallback)
 (action
  (run atdgen -j -j-strict-fields -j-std %{deps} -deriving-conv "eq,show")))

(rule
 (targets status_t.ml status_t.mli)
 (deps status.atd)
 (mode fallback)
 (action
  (run atdgen -t %{deps} -deriving-conv "eq,show")))

(rule
 (targets status_v.ml status_v.mli)
 (deps status.atd)
 (mode fallback)
 (action
  (run atdgen -v %{deps} -deriving-conv "eq,show")))

(rule
 (targets weapon_j.ml weapon_j.mli)
 (deps weapon.atd)
 (mode fallback)
 (action
  (run atdgen -j -j-strict-fields -j-std %{deps} -deriving-conv "eq,show")))

(rule
 (targets weapon_t.ml weapon_t.mli)
 (deps weapon.atd)
 (mode fallback)
 (action
  (run atdgen -t %{deps} -deriving-conv "eq,show")))

(rule
 (targets weapon_v.ml weapon_v.mli)
 (deps weapon.atd)
 (mode fallback)
 (action
  (run atdgen -v %{deps} -deriving-conv "eq,show")))

(library
 (public_name gamedata.data)
 (name data)
 (modules
  affix_j
  affix_t
  affix_v
  animation_j
  animation_t
  animation_v
  audio_j
  audio_t
  audio_v
  character_j
  character_t
  character_v
  common_j
  common_t
  common_v
  cursor_j
  cursor_t
  cursor_v
  drop_j
  drop_t
  drop_v
  entity_j
  entity_t
  entity_v
  equipment_j
  equipment_t
  equipment_v
  image_j
  image_t
  image_v
  model_j
  model_t
  model_v
  quality_j
  quality_t
  quality_v
  requirement_j
  requirement_t
  requirement_v
  skill_j
  skill_t
  skill_v
  stat_j
  stat_t
  stat_v
  status_j
  status_t
  status_v
  weapon_j
  weapon_t
  weapon_v
  entity_util
  validation)
 (libraries atd atdgen-runtime re base config util)
 (preprocess
  (pps ppx_deriving.eq ppx_deriving.show)))
