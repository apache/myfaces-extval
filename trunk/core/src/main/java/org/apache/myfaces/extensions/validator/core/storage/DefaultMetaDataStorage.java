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
import static org.apache.myfaces.extensions.validator.internal.UsageCategory.INTERNAL;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformationKeys;
import org.apache.myfaces.extensions.validator.core.property.DefaultPropertyInformation;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;

import java.util.Map;
import java.util.HashMap;

/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(INTERNAL)
public class DefaultMetaDataStorage implements MetaDataStorage
{
    private Map<String, PropertyInformation> cachedPropertyInformation = new HashMap<String, PropertyInformation>();

    public void storeMetaDataOf(PropertyInformation propertyInformation)
    {
        PropertyInformation propertyInformationToStore = new DefaultPropertyInformation();

        PropertyDetails propertyDetails = propertyInformation
                .getInformation(PropertyInformationKeys.PROPERTY_DETAILS, PropertyDetails.class);

        copyMetaData(propertyInformation, propertyInformationToStore);

        this.cachedPropertyInformation.put(
                createKey(propertyDetails.getBaseObject().getClass(), propertyDetails.getProperty()),
                propertyInformationToStore);
    }

    public MetaDataEntry[] getMetaData(Class targetClass, String targetProperty)
    {
        PropertyInformation propertyInformation = this.cachedPropertyInformation
                .get(createKey(targetClass, targetProperty));

        PropertyInformation clonedPropertyInformation = new DefaultPropertyInformation();
        copyMetaData(propertyInformation, clonedPropertyInformation);

        return clonedPropertyInformation.getMetaDataEntries();
    }

    public boolean containsMetaDataFor(Class targetClass, String targetProperty)
    {
        return this.cachedPropertyInformation.containsKey(createKey(targetClass, targetProperty));
    }

    private String createKey(Class targetClass, String targetProperty)
    {
        return targetClass.getName() + "#" + targetProperty;
    }

    private void copyMetaData(PropertyInformation source, PropertyInformation target)
    {
        MetaDataEntry newMetaDataEntry;
        for(MetaDataEntry metaDataEntry : source.getMetaDataEntries())
        {
            newMetaDataEntry = new MetaDataEntry();
            newMetaDataEntry.setKey(metaDataEntry.getKey());
            newMetaDataEntry.setValue(metaDataEntry.getValue());

            target.addMetaDataEntry(newMetaDataEntry);
        }
    }
}
