# Configuration constants for Premier Podcast Summary
# All configurable settings in one place

CONFIG <- list(
  # RSS Feed
  rss_url = "https://feeds.megaphone.fm/CORU5449522127",

  # Processing limits
  max_episodes = 7, # Initially process last 5 episodes

  # API endpoints
  assemblyai_base_url = "https://api.assemblyai.com/v2",
  anthropic_base_url = "https://api.anthropic.com/v1",

  # Claude settings
  anthropic_model = "claude-sonnet-4-5-20250929",
  anthropic_version = "2023-06-01",
  anthropic_max_tokens = 16384, # Increased for long transcripts (~45 min episodes)

  # Rate limiting (requests per minute)
  assemblyai_rate_limit = 10,
  anthropic_rate_limit = 50,

  # AssemblyAI polling settings
  assemblyai_poll_interval = 5, # seconds between status checks
  assemblyai_max_poll_attempts = 360, # max 30 minutes wait

  # Audio clip settings
  max_clips_per_episode = 10,
  clip_buffer_before = 1, # seconds before highlight
  clip_buffer_after = 1, # seconds after highlight

  # File paths (relative to project root)
  data_dir = "data/episodes",
  cache_dir = "cache",
  episodes_qmd_dir = "episodes",

  # Cache file names
  episodes_cache_file = "episodes_cache.json",
  rss_cache_file = "rss_feed_cache.json"
)

# Episode status values
EPISODE_STATUS <- list(
  pending = "pending",
  downloaded = "downloaded",
  transcribed = "transcribed",
  formatted = "formatted",
  analyzed = "analyzed",
  clipped = "clipped",
  complete = "complete",
  error = "error"
)
