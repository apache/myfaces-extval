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

import org.apache.myfaces.extensions.validator.crossval.CrossValidationStorageEntry;
import org.apache.myfaces.extensions.validator.crossval.annotation.RequiredIf;
import org.apache.myfaces.extensions.validator.crossval.annotation.RequiredIfType;
import org.apache.myfaces.extensions.validator.baseval.annotation.SkipValidationSupport;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import java.lang.annotation.Annotation;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@SkipValidationSupport
@UsageInformation(UsageCategory.INTERNAL)
public class RequiredIfStrategy extends AbstractCompareStrategy
{
    public boolean useTargetComponentToDisplayErrorMsg(CrossValidationStorageEntry crossValidationStorageEntry)
    {
        return false;
    }

    protected String getValidationErrorMsgKey(Annotation annotation, boolean isTargetComponent)
    {
        return ((RequiredIf) annotation).validationErrorMsgKey();
    }

    public boolean isViolation(Object object1, Object object2, Annotation annotation)
    {
        boolean violationFound = false;

        if (((RequiredIf) annotation).is().equals(RequiredIfType.empty))
        {
            violationFound = (object2 == null || object2.equals("")) && (object1 == null || object1.equals(""));
        }
        else if (((RequiredIf) annotation).is().equals(RequiredIfType.not_empty))
        {
            violationFound = (object2 != null && !object2.equals("")) && (object1 == null || object1.equals(""));
        }

        return violationFound;
    }

    public String[] getValidationTargets(Annotation annotation)
    {
        return ((RequiredIf) annotation).valueOf();
    }
}