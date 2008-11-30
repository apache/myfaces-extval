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
package org.apache.myfaces.extensions.validator.crossval.strategy;

import org.apache.myfaces.extensions.validator.crossval.CrossValidationStorage;
import org.apache.myfaces.extensions.validator.crossval.CrossValidationStorageEntry;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformationKeys;
import org.apache.myfaces.extensions.validator.util.ReflectionUtils;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;

/**
 * "[local_property.property1.property2]"
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
class LocalPropertyChainCompareStrategy extends LocalCompareStrategy
{
    @Override
    protected boolean tryToValidateLocally(CrossValidationStorageEntry crossValidationStorageEntry,
                                           CrossValidationStorage crossValidationStorage,
                                           String targetKey,
                                           AbstractCompareStrategy compareStrategy)
    {
        PropertyDetails propertyDetails = crossValidationStorageEntry.getMetaDataEntry()
            .getProperty(PropertyInformationKeys.PROPERTY_DETAILS, PropertyDetails.class);

        Object newBase = ReflectionUtils
            .getBaseOfPropertyChain(propertyDetails.getBaseObject(), targetKey);

        if(targetKey.contains("."))
        {
            //find the last property
            targetKey = targetKey.substring(targetKey.lastIndexOf(".") + 1, targetKey.length());
        }

        Object targetValue = getValueOfProperty(newBase, targetKey);

        boolean violationFound = false;

        if (compareStrategy.isViolation(crossValidationStorageEntry.getConvertedObject(),
                                targetValue, crossValidationStorageEntry.getMetaDataEntry().getValue(Annotation.class)))
        {

            CrossValidationStorageEntry tmpCrossValidationStorageEntry = new CrossValidationStorageEntry();
            tmpCrossValidationStorageEntry.setComponent(crossValidationStorageEntry.getComponent());
            tmpCrossValidationStorageEntry.setClientId(crossValidationStorageEntry.getClientId());
            tmpCrossValidationStorageEntry.setConvertedObject(targetValue);
            tmpCrossValidationStorageEntry.setValidationStrategy(compareStrategy);

            compareStrategy
                    .processTargetComponentAfterViolation(crossValidationStorageEntry, tmpCrossValidationStorageEntry);

            violationFound = true;
        }

        if (violationFound)
        {
            compareStrategy.processSourceComponentAfterViolation(crossValidationStorageEntry);
        }

        return true;
    }

    private Object getValueOfProperty(Object base, String property)
    {
        property = property.substring(0,1).toUpperCase() + property.substring(1, property.length());
        Method targetMethod = ReflectionUtils.tryToGetMethod(base.getClass(), "get" + property);

        if(targetMethod == null)
        {
            targetMethod = ReflectionUtils.tryToGetMethod(base.getClass(), "is" + property);
        }

        if(targetMethod == null)
        {
            throw new IllegalStateException(
                "class " + base.getClass() + " has no public get/is " + property.toLowerCase());
        }
        return ReflectionUtils.tryToInvokeMethod(base, targetMethod);
    }
}