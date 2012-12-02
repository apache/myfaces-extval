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

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

/**
 * Suggested interface for a group storage
 * used by the bvi module and add-ons
 * <p/>
 * it allows to manage groups for the current request
 * 
 * @since x.x.3
 */
@UsageInformation(UsageCategory.API)
public interface GroupStorage
{
    /**
     * Add the group, identified by the groupClass parameter, for the component identified by the clientId for the view
     * viewId.
     *
     * @param groupClass The group to add.
     * @param viewId The view where the uiComponent is located where we want to add the group.
     * @param clientId The clientId value of the component within the view.
     */
    void addGroup(Class groupClass, String viewId, String clientId);

    /**
     * Add the group in the restricted group list ,identified by the groupClass parameter, for the component identified
     * by the clientId for the view viewId.
     *
     * @param groupClass The group to add in the restricted group list.
     * @param viewId The view where the uiComponent is located where we want to add the group.
     * @param clientId The clientId value of the component within the view.
     */
    void restrictGroup(Class groupClass, String viewId, String clientId);

    /**
     * Gets the list of groups defined for the component with the clientId specified as parameter in the view.  The
     * array is the values of the groups added minus the groups from the restricted group list.
     * @param viewId The view where the uiComponent is located.
     * @param clientId The clientId value of the component within the view.
     * @return Array of groups defined for the component.
     */
    Class[] getGroups(String viewId, String clientId);

    /**
     * Allows to reset all groups which have been added so far.
     * @param viewId allows to reset groups only for one view-id
     * that allows to keep the groups e.g. of the current or the target view (depending on the given value).
     * if it is null, all stored view-ids will be affected.
     */
    void resetGroups(String viewId);

    /**
     * If the storage is locked, it isn't possible to change the state
     * @param viewId allows to lock groups only for one view-id
     * that allows to keep the groups e.g. of the current or the target view (depending on the given value)
     * if it is null, all stored view-ids will be affected.
     */
    void lockGroups(String viewId);

    /**
     * Allows to change the storage again 
     * @param viewId allows to lock groups only for one view-id
     * that allows to keep the groups e.g. of the current or the target view (depending on the given value)
     * if it is null, all stored view-ids will be affected.
     */
    void unlockGroups(String viewId);
}
