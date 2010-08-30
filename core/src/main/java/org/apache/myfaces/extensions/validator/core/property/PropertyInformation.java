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
package org.apache.myfaces.extensions.validator.core.property;

import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;

/**
 * Contains all the information on a property, like annotations, object and any custom information we want to keep.
 * MetaDataEntry's are considered as a special kind of information and separate methods are created for them.
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.API)
public interface PropertyInformation
{
    /**
     * Verifies if we have information for a certain key.  There are some predefined keys in PropertyInformationKeys.
     * @param key key of the information.
     * @return Is there information for that key maintained.
     */
    boolean containsInformation(String key);

    /**
     * Get the information for the specified key.  When there isn't any information available, it returns null.
     * @param key key of the information.
     * @return  Information linked to the key or null when no information is found for the key.
     */
    Object getInformation(String key);

    /**
     * Get the information for the specified key casted to a certain targetClass type.
     * @see PropertyInformation#getInformation(java.lang.String)
     * @param key key of the information.
     * @param targetClass target class type
     * @param <T> Type declaration for generics.
     * @return Information linked to the key or null when no information is found for the key.
     */
    <T> T getInformation(String key, Class<T> targetClass);

    /**
     * Assign the value as information to the key.
     * @param key key of the information.
     * @param value value as information for the key.
     */
    void setInformation(String key, Object value);

    /**
     * Get all the MetaDataEntry's stored.
     * @return MetaDataEntry's
     */
    MetaDataEntry[] getMetaDataEntries();

    /**
     * Add the MetaDataEntry to the internal structure.
     * @param metaDataEntry MetaDataEntry to store.
     */
    void addMetaDataEntry(MetaDataEntry metaDataEntry);

    /**
     * remove all MetaDataEntry's from the internal structure.
     */
    void resetMetaDataEntries();
}
