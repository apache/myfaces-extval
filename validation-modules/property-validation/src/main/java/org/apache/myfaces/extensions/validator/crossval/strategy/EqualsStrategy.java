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

import org.apache.myfaces.extensions.validator.baseval.annotation.SkipValidationSupport;
import org.apache.myfaces.extensions.validator.core.validation.EmptyValueAwareValidationStrategy;
import org.apache.myfaces.extensions.validator.core.validation.NullValueAwareValidationStrategy;
import org.apache.myfaces.extensions.validator.crossval.annotation.Equals;
import org.apache.myfaces.extensions.validator.crossval.annotation.MessageTarget;
import org.apache.myfaces.extensions.validator.crossval.parameter.CaseInsensitive;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

import java.lang.annotation.Annotation;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@SkipValidationSupport
@NullValueAwareValidationStrategy
@EmptyValueAwareValidationStrategy
@UsageInformation(UsageCategory.INTERNAL)
public class EqualsStrategy extends AbstractCompareStrategy
{
    protected MessageTarget getMessageTarget(Annotation annotation)
    {
        return ((Equals) annotation).validationErrorMsgTarget();
    }

    protected String getValidationErrorMsgKey(Annotation annotation, boolean isTargetComponent)
    {
        return ((Equals) annotation).validationErrorMsgKey();
    }

    public boolean isViolation(Object object1, Object object2, Annotation annotation)
    {
        if (object1 instanceof String && object2 instanceof String)
        {
            if (!isCaseSensitiveCompare(annotation))
            {
                return !((String) object1).equalsIgnoreCase((String) object2);
            }
        }
        return object1 != null && !object1.equals(object2);
    }

    public String[] getValidationTargets(Annotation annotation)
    {
        return ((Equals) annotation).value();
    }

    protected boolean isCaseSensitiveCompare(Annotation annotation)
    {
        return !ExtValUtils.getValidationParameterExtractor()
                .extract(annotation, ExtValUtils.getValidationParameterClassFor(CaseInsensitive.class))
                .iterator().hasNext();
    }
}