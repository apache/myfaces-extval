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
 * Contains all the information of a property
 * (e.g. {@link org.apache.myfaces.extensions.validator.core.property.PropertyDetails})
 * MetaDataEntry's are considered as a special kind of information and separate methods are created for them.
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.API)
public interface PropertyInformation
{
    /**
     * Verifies if we have information for the given key.
     * Some predefined keys are defined in {@link PropertyInformationKeys}.
     * @param key key of the information.
     * @return true if the instance is aware of an information which is linked to the given key
     */
    boolean containsInformation(String key);

    /**
     * Returns the information for the given key.
     * @param key key for the requested information
     * @return the object (or null) which is linked to the given key
     */
    Object getInformation(String key);

    /**
     * In addition to PropertyInformation#getInformation(java.lang.String) it casts to the given type.
     *
     * @see PropertyInformation#getInformation(java.lang.String)
     * @param key key for the requested information
     * @param targetClass target class type
     * @param <T> Type declaration for generics.
     * @return the object (or null) which is linked to the given key
     */
    <T> T getInformation(String key, Class<T> targetClass);

    /**
     * Stores the given value and links it to the given key.
     * @param key key of the information.
     * @param value value as information for the key.
     */
    void setInformation(String key, Object value);

    /**
     * Returns an immutable array which contains the
     * {@link org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry}s which were created for the property.
     * @return all {@link org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry}s
     */
    MetaDataEntry[] getMetaDataEntries();

    /**
     * Adds the given {@link org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry}
     * @param metaDataEntry {@link org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry} to store.
     */
    void addMetaDataEntry(MetaDataEntry metaDataEntry);

    /**
     * removes all {@link org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry}s.
     */
    void resetMetaDataEntries();
}
