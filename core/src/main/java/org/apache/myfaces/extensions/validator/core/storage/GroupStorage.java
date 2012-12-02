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
 * Interface for group storages e.g. used by the bv module (or add-ons which use validation groups).
 * <p/>
 * It allows to manage validation-groups for the current request.
 * Since JSF allows very dynamic pages, we can use the groups just for one request.
 * 
 * @since x.x.3
 */
@UsageInformation(UsageCategory.API)
public interface GroupStorage
{
    /**
     * Links the given group with the given component (id) for the a given view-id
     *
     * @param groupClass The group to add.
     * @param viewId The view where the uiComponent is located where we want to add the group.
     * @param clientId The clientId value of the component within the view.
     */
    void addGroup(Class groupClass, String viewId, String clientId);

    /**
     * Restricts the given group for the given component (id) for the a given view-id
     *
     * @param groupClass The group to add in the restricted group list.
     * @param viewId The view where the uiComponent is located where we want to add the group.
     * @param clientId The clientId value of the component within the view.
     */
    void restrictGroup(Class groupClass, String viewId, String clientId);

    /**
     * Returns all groups which are registered for the given component (id) in the given view.
     *
     * @param viewId The view where the uiComponent is located.
     * @param clientId The clientId value of the component within the view.
     * @return Array of groups defined for the component.
     */
    Class[] getGroups(String viewId, String clientId);
}
