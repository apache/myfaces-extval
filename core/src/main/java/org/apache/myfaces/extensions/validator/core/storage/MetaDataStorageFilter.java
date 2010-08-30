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
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;

/**
 * Allows a filtering of the MetaDataEntry's which are stored in the MetaDataStorage by the default implementation
 * (DefaultMetaDataStorage).  <br/>
 * The storage of the metaData is done as part of the extraction of them from the component (see method extract of
 * DefaultComponentMetaDataExtract).  So this extraction can return less information then which is available on the
 * property due to this filtering.
 *
 * @see org.apache.myfaces.extensions.validator.core.storage.DefaultMetaDataStorage
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(UsageCategory.API)
public interface MetaDataStorageFilter
{
    /**
     * Method should filter the metaDataEntry's of the propertyInformation which are not needed by the validation
     * process.
     * @param propertyInformation information about constraints on the property and contains the list of
     * MetaDataEntry's.
     */
    void filter(PropertyInformation propertyInformation);
}