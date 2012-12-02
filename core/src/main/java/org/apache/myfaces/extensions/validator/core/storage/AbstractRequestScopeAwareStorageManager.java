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
import static org.apache.myfaces.extensions.validator.internal.UsageCategory.REUSE;

import javax.faces.context.FacesContext;
import java.util.Map;
import java.util.HashMap;

/**
 * Generic storage manager implementation which stores the storage implementations in the JSF request scope.
 *
 * @since x.x.3
 */
@UsageInformation(REUSE)
public abstract class AbstractRequestScopeAwareStorageManager<T> extends AbstractStorageManager<T>
{
    /**
     * Returns a Map of all cached instances keyed on the type of storage available in the JSF Request scope.
     *
     * @return  Map of all cached storage Manager implementations keyed on the type of storage.
     */
    protected Map<String, T> resolveStorageMap()
    {
        Map requestMap = FacesContext.getCurrentInstance().getExternalContext().getRequestMap();
        Map<String, T> storageMap;

        if(!requestMap.containsKey(getStorageManagerKey()))
        {
            storageMap = new HashMap<String, T>();
            requestMap.put(getStorageManagerKey(), storageMap);
        }

        return (Map<String, T>)requestMap.get(getStorageManagerKey());
    }
}