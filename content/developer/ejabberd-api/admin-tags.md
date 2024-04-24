# API Tags

> This section enumerates the API tags of ejabberd [24.02](../../archive/24.02/index.md).  If you are using an old ejabberd release, please refer to the corresponding archived version of this page in the [Archive](../../archive/index.md). 



## accounts


* [ban_account](admin-api.md#ban_account)


* [change_password](admin-api.md#change_password)


* [check_account](admin-api.md#check_account)


* [check_password](admin-api.md#check_password)


* [check_password_hash](admin-api.md#check_password_hash)


* [delete_old_users](admin-api.md#delete_old_users)


* [delete_old_users_vhost](admin-api.md#delete_old_users_vhost)


* [register](admin-api.md#register)


* [registered_users](admin-api.md#registered_users)


* [unban_ip](admin-api.md#unban_ip)


* [unregister](admin-api.md#unregister)


## acme


* [list_certificates](admin-api.md#list_certificates)


* [request_certificate](admin-api.md#request_certificate)


* [revoke_certificate](admin-api.md#revoke_certificate)


## cluster


* [join_cluster](admin-api.md#join_cluster)


* [leave_cluster](admin-api.md#leave_cluster)


* [list_cluster](admin-api.md#list_cluster)


* [set_master](admin-api.md#set_master)


## config


* [convert_to_yaml](admin-api.md#convert_to_yaml)


* [dump_config](admin-api.md#dump_config)


* [reload_config](admin-api.md#reload_config)


## documentation


* [gen_html_doc_for_commands](admin-api.md#gen_html_doc_for_commands)


* [gen_markdown_doc_for_commands](admin-api.md#gen_markdown_doc_for_commands)


* [gen_markdown_doc_for_tags](admin-api.md#gen_markdown_doc_for_tags)


* [man](admin-api.md#man)


## ejabberdctl


* [help](admin-api.md#help)


* [mnesia_info_ctl](admin-api.md#mnesia_info_ctl)


* [print_sql_schema](admin-api.md#print_sql_schema)


## erlang


* [compile](admin-api.md#compile)


* [get_cookie](admin-api.md#get_cookie)


* [restart_module](admin-api.md#restart_module)


## last


* [get_last](admin-api.md#get_last)


* [set_last](admin-api.md#set_last)


## logs


* [get_loglevel](admin-api.md#get_loglevel)


* [reopen_log](admin-api.md#reopen_log)


* [rotate_log](admin-api.md#rotate_log)


* [set_loglevel](admin-api.md#set_loglevel)


## mam


* [remove_mam_for_user](admin-api.md#remove_mam_for_user)


* [remove_mam_for_user_with_peer](admin-api.md#remove_mam_for_user_with_peer)


## mnesia


* [backup](admin-api.md#backup)


* [delete_mnesia](admin-api.md#delete_mnesia)


* [dump](admin-api.md#dump)


* [dump_table](admin-api.md#dump_table)


* [export2sql](admin-api.md#export2sql)


* [export_piefxis](admin-api.md#export_piefxis)


* [export_piefxis_host](admin-api.md#export_piefxis_host)


* [import_dir](admin-api.md#import_dir)


* [import_file](admin-api.md#import_file)


* [import_piefxis](admin-api.md#import_piefxis)


* [import_prosody](admin-api.md#import_prosody)


* [install_fallback](admin-api.md#install_fallback)


* [load](admin-api.md#load)


* [mnesia_change_nodename](admin-api.md#mnesia_change_nodename)


* [mnesia_info](admin-api.md#mnesia_info)


* [mnesia_info_ctl](admin-api.md#mnesia_info_ctl)


* [mnesia_table_info](admin-api.md#mnesia_table_info)


* [restore](admin-api.md#restore)


## modules


* [module_check](admin-api.md#module_check)


* [module_install](admin-api.md#module_install)


* [module_uninstall](admin-api.md#module_uninstall)


* [module_upgrade](admin-api.md#module_upgrade)


* [modules_available](admin-api.md#modules_available)


* [modules_installed](admin-api.md#modules_installed)


* [modules_update_specs](admin-api.md#modules_update_specs)


## muc


* [create_rooms_file](admin-api.md#create_rooms_file)


* [destroy_rooms_file](admin-api.md#destroy_rooms_file)


* [get_user_rooms](admin-api.md#get_user_rooms)


* [get_user_subscriptions](admin-api.md#get_user_subscriptions)


* [muc_online_rooms](admin-api.md#muc_online_rooms)


* [muc_online_rooms_by_regex](admin-api.md#muc_online_rooms_by_regex)


* [muc_register_nick](admin-api.md#muc_register_nick)


* [muc_unregister_nick](admin-api.md#muc_unregister_nick)


* [rooms_empty_destroy](admin-api.md#rooms_empty_destroy)


* [rooms_empty_list](admin-api.md#rooms_empty_list)


* [rooms_unused_destroy](admin-api.md#rooms_unused_destroy)


* [rooms_unused_list](admin-api.md#rooms_unused_list)


## muc_room


* [change_room_option](admin-api.md#change_room_option)


* [create_room](admin-api.md#create_room)


* [create_room_with_opts](admin-api.md#create_room_with_opts)


* [destroy_room](admin-api.md#destroy_room)


* [get_room_affiliation](admin-api.md#get_room_affiliation)


* [get_room_affiliations](admin-api.md#get_room_affiliations)


* [get_room_history](admin-api.md#get_room_history)


* [get_room_occupants](admin-api.md#get_room_occupants)


* [get_room_occupants_number](admin-api.md#get_room_occupants_number)


* [get_room_options](admin-api.md#get_room_options)


* [get_subscribers](admin-api.md#get_subscribers)


* [send_direct_invitation](admin-api.md#send_direct_invitation)


* [set_room_affiliation](admin-api.md#set_room_affiliation)


* [subscribe_room](admin-api.md#subscribe_room)


* [subscribe_room_many](admin-api.md#subscribe_room_many)


* [unsubscribe_room](admin-api.md#unsubscribe_room)


## muc_sub


* [create_room_with_opts](admin-api.md#create_room_with_opts)


* [get_subscribers](admin-api.md#get_subscribers)


* [get_user_subscriptions](admin-api.md#get_user_subscriptions)


* [subscribe_room](admin-api.md#subscribe_room)


* [subscribe_room_many](admin-api.md#subscribe_room_many)


* [unsubscribe_room](admin-api.md#unsubscribe_room)


## oauth


* [oauth_add_client_implicit](admin-api.md#oauth_add_client_implicit)


* [oauth_add_client_password](admin-api.md#oauth_add_client_password)


* [oauth_issue_token](admin-api.md#oauth_issue_token)


* [oauth_list_tokens](admin-api.md#oauth_list_tokens)


* [oauth_remove_client](admin-api.md#oauth_remove_client)


* [oauth_revoke_token](admin-api.md#oauth_revoke_token)


## offline


* [get_offline_count](admin-api.md#get_offline_count)


## private


* [bookmarks_to_pep](admin-api.md#bookmarks_to_pep)


* [private_get](admin-api.md#private_get)


* [private_set](admin-api.md#private_set)


## purge


* [abort_delete_old_mam_messages](admin-api.md#abort_delete_old_mam_messages)


* [abort_delete_old_messages](admin-api.md#abort_delete_old_messages)


* [delete_expired_messages](admin-api.md#delete_expired_messages)


* [delete_expired_pubsub_items](admin-api.md#delete_expired_pubsub_items)


* [delete_old_mam_messages](admin-api.md#delete_old_mam_messages)


* [delete_old_mam_messages_batch](admin-api.md#delete_old_mam_messages_batch)


* [delete_old_mam_messages_status](admin-api.md#delete_old_mam_messages_status)


* [delete_old_messages](admin-api.md#delete_old_messages)


* [delete_old_messages_batch](admin-api.md#delete_old_messages_batch)


* [delete_old_messages_status](admin-api.md#delete_old_messages_status)


* [delete_old_pubsub_items](admin-api.md#delete_old_pubsub_items)


* [delete_old_push_sessions](admin-api.md#delete_old_push_sessions)


* [delete_old_users](admin-api.md#delete_old_users)


* [delete_old_users_vhost](admin-api.md#delete_old_users_vhost)


## roster


* [add_rosteritem](admin-api.md#add_rosteritem)


* [delete_rosteritem](admin-api.md#delete_rosteritem)


* [get_roster](admin-api.md#get_roster)


* [process_rosteritems](admin-api.md#process_rosteritems)


* [push_alltoall](admin-api.md#push_alltoall)


* [push_roster](admin-api.md#push_roster)


* [push_roster_all](admin-api.md#push_roster_all)


## s2s


* [incoming_s2s_number](admin-api.md#incoming_s2s_number)


* [outgoing_s2s_number](admin-api.md#outgoing_s2s_number)


* [stop_s2s_connections](admin-api.md#stop_s2s_connections)


## server


* [clear_cache](admin-api.md#clear_cache)


* [gc](admin-api.md#gc)


* [halt](admin-api.md#halt)


* [registered_vhosts](admin-api.md#registered_vhosts)


* [restart](admin-api.md#restart)


* [status](admin-api.md#status)


* [stop](admin-api.md#stop)


* [stop_kindly](admin-api.md#stop_kindly)


* [update](admin-api.md#update)


* [update_list](admin-api.md#update_list)


## session


* [connected_users](admin-api.md#connected_users)


* [connected_users_info](admin-api.md#connected_users_info)


* [connected_users_number](admin-api.md#connected_users_number)


* [connected_users_vhost](admin-api.md#connected_users_vhost)


* [get_presence](admin-api.md#get_presence)


* [kick_session](admin-api.md#kick_session)


* [kick_user](admin-api.md#kick_user)


* [num_resources](admin-api.md#num_resources)


* [resource_num](admin-api.md#resource_num)


* [set_presence](admin-api.md#set_presence)


* [status_list](admin-api.md#status_list)


* [status_list_host](admin-api.md#status_list_host)


* [status_num](admin-api.md#status_num)


* [status_num_host](admin-api.md#status_num_host)


* [user_resources](admin-api.md#user_resources)


* [user_sessions_info](admin-api.md#user_sessions_info)


## shared_roster_group


* [srg_create](admin-api.md#srg_create)


* [srg_delete](admin-api.md#srg_delete)


* [srg_get_info](admin-api.md#srg_get_info)


* [srg_get_members](admin-api.md#srg_get_members)


* [srg_list](admin-api.md#srg_list)


* [srg_user_add](admin-api.md#srg_user_add)


* [srg_user_del](admin-api.md#srg_user_del)


## sql


* [convert_to_scram](admin-api.md#convert_to_scram)


* [import_prosody](admin-api.md#import_prosody)


* [print_sql_schema](admin-api.md#print_sql_schema)


## stanza


* [privacy_set](admin-api.md#privacy_set)


* [send_message](admin-api.md#send_message)


* [send_stanza](admin-api.md#send_stanza)


* [send_stanza_c2s](admin-api.md#send_stanza_c2s)


## statistics


* [connected_users_number](admin-api.md#connected_users_number)


* [incoming_s2s_number](admin-api.md#incoming_s2s_number)


* [outgoing_s2s_number](admin-api.md#outgoing_s2s_number)


* [stats](admin-api.md#stats)


* [stats_host](admin-api.md#stats_host)


* [status_num](admin-api.md#status_num)


* [status_num_host](admin-api.md#status_num_host)


## v1


* [add_rosteritem](admin-api.md#add_rosteritem)


* [oauth_issue_token](admin-api.md#oauth_issue_token)


* [send_direct_invitation](admin-api.md#send_direct_invitation)


* [set_presence](admin-api.md#set_presence)


* [srg_create](admin-api.md#srg_create)


* [subscribe_room](admin-api.md#subscribe_room)


* [subscribe_room_many](admin-api.md#subscribe_room_many)


## vcard


* [get_vcard](admin-api.md#get_vcard)


* [get_vcard2](admin-api.md#get_vcard2)


* [get_vcard2_multi](admin-api.md#get_vcard2_multi)


* [set_nickname](admin-api.md#set_nickname)


* [set_vcard](admin-api.md#set_vcard)


* [set_vcard2](admin-api.md#set_vcard2)


* [set_vcard2_multi](admin-api.md#set_vcard2_multi)
