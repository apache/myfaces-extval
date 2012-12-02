/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.extensions.validator.core.storage;

import org.apache.myfaces.extensions.validator.util.GroupUtils;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import static org.apache.myfaces.extensions.validator.internal.UsageCategory.INTERNAL;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

/**
 * default storage implementation for groups
 *
 * @since x.x.3
 */
@UsageInformation(INTERNAL)
public class DefaultGroupStorage implements GroupStorage
{
    protected final Logger logger = Logger.getLogger(getClass().getName());

    private Map<String, List<Class>> addedGroups = new HashMap<String, List<Class>>();

    private Map<String, List<Class>> restrictedGroups = new HashMap<String, List<Class>>();

    private Map<String, Boolean> lockedGroupsPerView = new HashMap<String, Boolean>();

    public void addGroup(Class groupClass, String viewId, String clientId)
    {
        if(Boolean.TRUE.equals(this.lockedGroupsPerView.get(viewId)))
        {
            return;
        }
        addGroupToGroupStorage(groupClass, viewId, clientId, this.addedGroups);
    }

    public void restrictGroup(Class groupClass, String viewId, String clientId)
    {
        if(Boolean.TRUE.equals(this.lockedGroupsPerView.get(viewId)))
        {
            return;
        }
        addGroupToGroupStorage(groupClass, viewId, clientId, this.restrictedGroups);
    }

    public Class[] getGroups(String viewId, String clientId)
    {
        if(this.addedGroups.size() < 1)
        {
            return null;
        }

        //add found groups
        String key = GroupUtils.getGroupKey(viewId, null);
        List<Class> resultListForPage = buildGroupList(key, this.addedGroups);

        key = GroupUtils.getGroupKey(viewId, clientId);
        List<Class> resultListForComponent = buildGroupList(key, this.addedGroups);

        //remove restricted groups
        Class[] resultsForPage =
                filterGroupList(GroupUtils.getGroupKey(viewId, null), resultListForPage);
        Class[] resultsForComponent =
                filterGroupList(GroupUtils.getGroupKey(viewId, clientId), resultListForComponent);

        if(resultsForPage.length == 0)
        {
            if(resultsForComponent.length == 0)
            {
                this.logger.fine("no groups for group-validation available." +
                        "maybe you restricted all groups or you aren't using groups." +
                        "bean validation will use the default group for validation");
            }
            return resultsForComponent;
        }
        else if(resultsForComponent.length == 0)
        {
            return resultsForPage;
        }

        return mergeResults(resultsForPage, resultsForComponent);
    }

    public void resetGroups(String viewId)
    {
        resetGroupsForList(this.addedGroups, viewId);
        resetGroupsForList(this.restrictedGroups, viewId);
    }

    private void resetGroupsForList(Map<String, List<Class>> groupList, String viewId)
    {
        if(viewId == null)
        {
            groupList.clear();
            return;
        }

        for(String currentKey : groupList.keySet())
        {
            if(currentKey.equals(viewId) || currentKey.startsWith(viewId + "@"))
            {
                groupList.put(currentKey, null);
            }
        }
    }

    public void lockGroups(String viewId)
    {
        this.lockedGroupsPerView.put(viewId, Boolean.TRUE);
    }

    public void unlockGroups(String viewId)
    {
        this.lockedGroupsPerView.put(viewId, null);
    }

    private void addGroupToGroupStorage(Class groupClass, String viewId, String clientId,
                                        Map<String, List<Class>> groupStorage)
    {
        List<Class> groupList = groupStorage.get(GroupUtils.getGroupKey(viewId, clientId));

        if(groupList == null)
        {
            groupList = new ArrayList<Class>();
            groupStorage.put(GroupUtils.getGroupKey(viewId, clientId), groupList);
        }

        if(!groupList.contains(groupClass))
        {
            groupList.add(groupClass);
        }
    }

    private List<Class> buildGroupList(String key, Map<String, List<Class>> groupStorage)
    {
        List<Class> list = groupStorage.get(key);
        return (list != null) ? list : new ArrayList<Class>();
    }

    private Class[] filterGroupList(String key, List<Class> addedGroups)
    {
        List<Class> restrictedGroups = buildGroupList(key, this.restrictedGroups);
        List<Class> results = new ArrayList<Class>();

        for(Class currentGroup : addedGroups)
        {
            if(!restrictedGroups.contains(currentGroup))
            {
                results.add(currentGroup);
            }
        }

        return results.toArray(new Class[results.size()]);
    }

    private Class[] mergeResults(Class[] resultsForPage, Class[] resultsForComponent)
    {
        Class[] mergedResult = new Class[resultsForPage.length + resultsForComponent.length];

        System.arraycopy(resultsForPage, 0, mergedResult, 0, resultsForPage.length);
        System.arraycopy(resultsForComponent, 0, mergedResult, resultsForPage.length, resultsForComponent.length);

        return mergedResult;
    }
}
