# All files in the 'lib' directory will be loaded
# before nanoc starts compiling.
include Nanoc::Helpers::Rendering, Nanoc::Helpers::Blogging, BlogEntriesHelper

module PostHelper
  def get_post_start(post)
    content = post.compiled_content
    if content =~ /\s<!-- more -->\s/
      content = content.partition('<!-- more -->').first +
          "&bull;&nbsp;&bull;&nbsp;&bull; &nbsp;&nbsp; <div class='read-more'><a href='#{post.path}'>Continue reading &rsaquo;</a></div>"
    end
    return content
  end
end

include PostHelper