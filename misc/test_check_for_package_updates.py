"""Regression tests for GitHub release selection."""

import unittest
from unittest.mock import Mock, patch

import check_for_package_updates as checker


class GitHubReleaseTests(unittest.TestCase):
    def select(self, releases, prereleases=False, pin=None):
        latest_missing = Mock(ok=False, status_code=404)
        release_list = Mock(ok=True, status_code=200)
        release_list.json.return_value = releases
        responses = (
            [release_list] if prereleases or pin else [latest_missing, release_list]
        )
        with patch.object(checker.requests, "get", side_effect=responses):
            return checker.get_latest_github_release(
                "example/package", "test-token", prereleases, False, version_pin=pin
            )

    def test_pinned_release_skips_prerelease(self):
        releases = [
            {"tag_name": "v4.2.10rc1", "prerelease": True},
            {"tag_name": "v4.2.9", "prerelease": False},
        ]
        self.assertEqual(self.select(releases, pin="4.2"), "v4.2.9")
        self.assertEqual(
            self.select(releases, prereleases=True, pin="4.2"), "v4.2.10rc1"
        )

    def test_fallback_skips_prerelease(self):
        releases = [
            {"tag_name": "v2.0rc1", "prerelease": True},
            {"tag_name": "v1.9", "prerelease": False},
        ]
        self.assertEqual(self.select(releases), "v1.9")
        self.assertEqual(self.select(releases, prereleases=True), "v2.0rc1")

    def test_only_prereleases(self):
        releases = [{"tag_name": "v4.2.10rc1", "prerelease": True}]
        self.assertIsNone(self.select(releases))
        self.assertIsNone(self.select(releases, pin="4.2"))

    def test_pin_does_not_select_another_series(self):
        releases = [
            {"tag_name": "v5.0", "prerelease": False},
            {"tag_name": "v4.2.10rc1", "prerelease": True},
        ]
        self.assertIsNone(self.select(releases, pin="4.2"))

    def test_empty_releases(self):
        self.assertIsNone(self.select([]))


if __name__ == "__main__":
    unittest.main()
