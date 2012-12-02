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

import org.apache.myfaces.extensions.validator.crossval.annotation.EmptyIf;
import org.apache.myfaces.extensions.validator.crossval.annotation.EmptyIfType;
import org.apache.myfaces.extensions.validator.crossval.annotation.MessageTarget;

/**
 */
public class EmptyIfStrategy extends AbstractCompareStrategy<EmptyIf>
{
    protected MessageTarget getMessageTarget(EmptyIf annotation)
    {
        return annotation.validationErrorMsgTarget();
    }

    @Override
    protected String getValidationErrorMsgKey(EmptyIf annotation, boolean isTargetComponent)
    {
        return annotation.validationErrorMsgKey();
    }

    @Override
    public String[] getValidationTargets(EmptyIf annotation)
    {
        return annotation.valueOf();
    }

    @Override
    public boolean isViolation(Object source, Object target, EmptyIf annotation)
    {
        boolean violationFound = false;

        if (annotation.is().equals(EmptyIfType.empty))
        {
            violationFound = (isTargetEmpty(target) || Boolean.FALSE.equals(target)) && !isSourceEmpty(source);
        }
        else if (annotation.is().equals(EmptyIfType.not_empty))
        {
            violationFound = (!isSourceEmpty(source) && !isTargetEmpty(target) && !(target instanceof Boolean)) ||
                    (Boolean.TRUE.equals(target) && !isSourceEmpty(source));
        }

        return violationFound;
    }

    private boolean isTargetEmpty(Object target)
    {
        return target == null || target.equals("");
    }

    private boolean isSourceEmpty(Object source)
    {
        return source == null || source.equals("");
    }
}