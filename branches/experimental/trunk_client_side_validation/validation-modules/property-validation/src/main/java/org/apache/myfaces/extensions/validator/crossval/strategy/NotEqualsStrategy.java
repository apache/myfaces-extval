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

import org.apache.myfaces.extensions.validator.crossval.annotation.NotEquals;

import java.lang.annotation.Annotation;

/**
 * @author Gerhard Petracek
 */
public class NotEqualsStrategy extends EqualsStrategy
{

    @Override
    protected String getValidationErrorMsgKey(Annotation annotation,
            boolean isTargetComponent)
    {
        return ((NotEquals) annotation).validationErrorMsgKey();
    }

    @Override
    public boolean isViolation(Object object1, Object object2,
            Annotation annotation)
    {
        return !super.isViolation(object1, object2, annotation);
    }

    @Override
    public String[] getValidationTargets(Annotation annotation)
    {
        return ((NotEquals) annotation).value();
    }
}
